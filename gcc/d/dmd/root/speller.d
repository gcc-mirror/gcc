/**
 * Spell checker
 *
 * Does not have any dependencies on the rest of DMD.
 *
 * Copyright: Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, https://www.digitalmars.com
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/root/speller.d, root/_speller.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_speller.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/root/speller.d
 */

module dmd.root.speller;

/**************************************************
 * Looks for correct spelling.
 * Looks a distance of up to two.
 * This does an exhaustive search, so can potentially be very slow.
 * Params:
 *      seed = wrongly spelled word
 *      dg = search delegate of the form `T delegate(const(char)[] p, out int cost)`
 * Returns:
 *      T.init = no correct spellings found,
 *      otherwise the value returned by dg() for first possible correct spelling
 */
auto speller(alias dg)(const(char)[] seed)
if (isSearchFunction!dg)
{
    const size_t maxdist = seed.length < 4 ? seed.length / 2 : 2;
    foreach (distance; 0 .. maxdist)
    {
        if (auto p = spellerX!dg(seed, distance != 0))
            return p;
        //      if (seedlen > 10)
        //          break;
    }
    return null; // didn't find it
}

private:

import core.stdc.stdlib;
import core.stdc.string;
import dmd.common.smallbuffer : SmallBuffer;

enum isSearchFunction(alias fun) = is(searchFunctionType!fun);
alias searchFunctionType(alias fun) = typeof(() {int x; return fun("", x);}());

/*************************************
 * Spell check level 1.
 * Params:
 *      dg = delegate that looks up string in dictionary AA and returns value found
 *      seed = starting string
 *      flag = if true, do 2 level lookup otherwise 1 level
 * Returns:
 *      whatever dg returns, null if no match
 */
auto spellerX(alias dg)(const(char)[] seed, bool flag)
{
    if (!seed.length)
        return null;

    /* Need buffer to store trial strings in
     */
    char[30] tmp = void;
    auto sb = SmallBuffer!char(seed.length + 1, tmp[]);
    char[] buf = sb[];

    int cost = int.max;
    searchFunctionType!dg p = null;

    /* Deletions */
    buf[0 .. seed.length - 1] = seed[1 .. $];
    foreach (i; 0 .. seed.length)
    {
        //printf("del buf = '%s'\n", buf);
        int ncost;
        auto np = flag ? spellerY!dg(buf[0 .. seed.length - 1], i, ncost)
                       : dg(buf[0 .. seed.length - 1], ncost);
        if (combineSpellerResult(p, cost, np, ncost))
            return p;
        buf[i] = seed[i];
    }

    /* Transpositions */
    if (!flag)
    {
        buf[0 .. seed.length] = seed;
        for (size_t i = 0; i + 1 < seed.length; i++)
        {
            // swap [i] and [i + 1]
            buf[i] = seed[i + 1];
            buf[i + 1] = seed[i];
            //printf("tra buf = '%s'\n", buf);
            int ncost;
            auto np = dg(buf[0 .. seed.length], ncost);
            if (combineSpellerResult(p, cost, np, ncost))
                return p;
            buf[i] = seed[i];
        }
    }

    /* Substitutions */
    buf[0 .. seed.length] = seed;
    foreach (i; 0 .. seed.length)
    {
        foreach (s; idchars)
        {
            buf[i] = s;
            //printf("sub buf = '%s'\n", buf);
            int ncost;
            auto np = flag ? spellerY!dg(buf[0 .. seed.length], i + 1, ncost)
                           : dg(buf[0 .. seed.length], ncost);
            if (combineSpellerResult(p, cost, np, ncost))
                return p;
        }
        buf[i] = seed[i];
    }

    /* Insertions */
    buf[1 .. seed.length + 1] = seed;
    foreach (i; 0 .. seed.length + 1) // yes, do seed.length+1 iterations
    {
        foreach (s; idchars)
        {
            buf[i] = s;
            //printf("ins buf = '%s'\n", buf);
            int ncost;
            auto np = flag ? spellerY!dg(buf[0 .. seed.length + 1], i + 1, ncost)
                           : dg(buf[0 .. seed.length + 1], ncost);
            if (combineSpellerResult(p, cost, np, ncost))
                return p;
        }
        if (i < seed.length)
            buf[i] = seed[i];
    }

    return p; // return "best" result
}

/**********************************************
 * Do second level of spell matching.
 * Params:
 *      dg = delegate that looks up string in dictionary AA and returns value found
 *      seed = starting string
 *      index = index into seed[] that is where we will mutate it
 *      cost = set to cost of match
 * Returns:
 *      whatever dg returns, null if no match
 */
auto spellerY(alias dg)(const(char)[] seed, size_t index, out int cost)
{
    if (!seed.length)
        return null;

    /* Allocate a buf to store the new string to play with, needs
     * space for an extra char for insertions
     */
    char[30] tmp = void;        // stack allocations are fastest
    auto sb = SmallBuffer!char(seed.length + 1, tmp[]);
    char[] buf = sb[];
    buf[0 .. index] = seed[0 .. index];

    cost = int.max;             // start with worst possible match
    searchFunctionType!dg p = null;

    /* Delete character at seed[index] */
    if (index < seed.length)
    {
        buf[index .. seed.length - 1] = seed[index + 1 .. $]; // seed[] with deleted character
        int ncost;
        auto np = dg(buf[0 .. seed.length - 1], ncost); // look it up
        if (combineSpellerResult(p, cost, np, ncost))   // compare with prev match
            return p;                                   // cannot get any better
    }

    /* Substitute character at seed[index] */
    if (index < seed.length)
    {
        buf[0 .. seed.length] = seed;
        foreach (s; idchars)
        {
            buf[index] = s;     // seed[] with substituted character
            //printf("sub buf = '%s'\n", buf);
            int ncost;
            auto np = dg(buf[0 .. seed.length], ncost);
            if (combineSpellerResult(p, cost, np, ncost))
                return p;
        }
    }

    /* Insert character at seed[index] */
    buf[index + 1 .. seed.length + 1] = seed[index .. $];
    foreach (s; idchars)
    {
        buf[index] = s;
        //printf("ins buf = '%s'\n", buf);
        int ncost;
        auto np = dg(buf[0 .. seed.length + 1], ncost);
        if (combineSpellerResult(p, cost, np, ncost))
            return p;
    }
    return p; // return "best" result
}


/* Characters used to substitute ones in the string we're checking
 * the spelling on.
 */
immutable string idchars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

/**************************************************
 * Combine a new result from the spell checker to
 * find the one with the closest symbol with
 * respect to the cost defined by the search function
 * Params:
 *      p = best found spelling so far, T.init if none found yet.
 *          If np is better, p is replaced with np
 *      cost = cost of p (int.max if none found yet).
 *          If np is better, cost is replaced with ncost
 *      np = current spelling to check against p, T.init if none
 *      ncost = cost of np if np is not T.init
 * Returns:
 *      true    if the cost is less or equal 0, meaning we can stop looking
 *      false   otherwise
 */
bool combineSpellerResult(T)(ref T p, ref int cost, T np, int ncost)
{
    if (np && ncost < cost) // if np is better
    {
        p = np;             // np is new best match
        cost = ncost;
        if (cost <= 0)
            return true;    // meaning we can stop looking
    }
    return false;
}

/************************************* Tests ***********************/

unittest
{
    static immutable string[][] cases =
    [
        ["hello", "hell", "y"],
        ["hello", "hel", "y"],
        ["hello", "ello", "y"],
        ["hello", "llo", "y"],
        ["hello", "hellox", "y"],
        ["hello", "helloxy", "y"],
        ["hello", "xhello", "y"],
        ["hello", "xyhello", "y"],
        ["hello", "ehllo", "y"],
        ["hello", "helol", "y"],
        ["hello", "abcd", "n"],
        ["hello", "helxxlo", "y"],
        ["hello", "ehlxxlo", "n"],
        ["hello", "heaao", "y"],
        ["_123456789_123456789_123456789_123456789", "_123456789_123456789_123456789_12345678", "y"],
    ];
    //printf("unittest_speller()\n");

    string dgarg;

    string speller_test(const(char)[] s, ref int cost)
    {
        assert(s[$-1] != '\0');
        //printf("speller_test(%s, %s)\n", dgarg, s);
        cost = 0;
        if (dgarg == s)
            return dgarg;
        return null;
    }

    dgarg = "hell";
    auto p = speller!speller_test("hello");
    assert(p !is null);
    foreach (testCase; cases)
    {
        //printf("case [%d]\n", i);
        dgarg = testCase[1];
        auto p2 = speller!speller_test(testCase[0]);
        if (p2)
            assert(testCase[2][0] == 'y');
        else
            assert(testCase[2][0] == 'n');
    }
    //printf("unittest_speller() success\n");
}
