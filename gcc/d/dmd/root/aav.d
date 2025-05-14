/**
 * Associative array implementation.
 *
 * Copyright: Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, https://www.digitalmars.com
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/root/aav.d, root/_aav.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_aav.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/root/aav.d
 */

module dmd.root.aav;

import core.stdc.string;
import dmd.root.rmem;

nothrow:

private size_t hash(size_t a) pure nothrow @nogc @safe
{
    a ^= (a >> 20) ^ (a >> 12);
    return a ^ (a >> 7) ^ (a >> 4);
}

private struct KeyValueTemplate(K,V)
{
    K key;
    V value;
}

alias Key = void*;
alias Value = void*;

alias KeyValue = KeyValueTemplate!(Key, Value);

private struct aaA
{
private:
    aaA* next;
    KeyValue keyValue;
    alias keyValue this;
}

private struct AA
{
private:
    aaA** b;
    size_t b_length;
    size_t nodes; // total number of aaA nodes
    aaA*[4] binit; // initial value of b[]
    aaA aafirst; // a lot of these AA's have only one entry
}

/****************************************************
 * Determine number of entries in associative array.
 */
private size_t dmd_aaLen(const AA* aa) pure nothrow @nogc @safe
{
    return aa ? aa.nodes : 0;
}

/*************************************************
 * Get pointer to value in associative array indexed by key.
 * Add entry for key if it is not already there, returning a pointer to a null Value.
 * Create the associative array if it does not already exist.
 */
private Value* dmd_aaGet(AA** paa, Key key) pure nothrow
{
    //printf("paa = %p\n", paa);
    if (!*paa)
    {
        AA* a = cast(AA*)mem.xmalloc(AA.sizeof);
        a.b = cast(aaA**)a.binit;
        a.b_length = 4;
        a.nodes = 0;
        a.binit[0] = null;
        a.binit[1] = null;
        a.binit[2] = null;
        a.binit[3] = null;
        *paa = a;
        assert((*paa).b_length == 4);
    }
    //printf("paa = %p, *paa = %p\n", paa, *paa);
    assert((*paa).b_length);
    size_t i = hash(cast(size_t)key) & ((*paa).b_length - 1);
    aaA** pe = &(*paa).b[i];
    aaA* e;
    while ((e = *pe) !is null)
    {
        if (key == e.key)
            return &e.value;
        pe = &e.next;
    }
    // Not found, create new elem
    //printf("create new one\n");
    size_t nodes = ++(*paa).nodes;
    e = (nodes != 1) ? cast(aaA*)mem.xmalloc(aaA.sizeof) : &(*paa).aafirst;
    //e = new aaA();
    e.next = null;
    e.key = key;
    e.value = null;
    *pe = e;
    //printf("length = %d, nodes = %d\n", (*paa)->b_length, nodes);
    if (nodes > (*paa).b_length * 2)
    {
        //printf("rehash\n");
        dmd_aaRehash(paa);
    }
    return &e.value;
}

/*************************************************
 * Get value in associative array indexed by key.
 * Returns NULL if it is not already there.
 */
private Value dmd_aaGetRvalue(AA* aa, Key key) pure nothrow @nogc
{
    //printf("_aaGetRvalue(key = %p)\n", key);
    if (aa)
    {
        size_t i;
        size_t len = aa.b_length;
        i = hash(cast(size_t)key) & (len - 1);
        aaA* e = aa.b[i];
        while (e)
        {
            if (key == e.key)
                return e.value;
            e = e.next;
        }
    }
    return null; // not found
}

/**
Gets a range of key/values for `aa`.

Returns: a range of key/values for `aa`.
*/
@property auto asRange(AA* aa) pure nothrow @nogc
{
    return AARange!(Key, Value)(aa);
}

private struct AARange(K,V)
{
    AA* aa;
    // current index into bucket array `aa.b`
    size_t bIndex;
    aaA* current;

    this(AA* aa) pure nothrow @nogc scope
    {
        if (aa)
        {
            this.aa = aa;
            toNext();
        }
    }

    @property bool empty() const pure nothrow @nogc @safe
    {
        return current is null;
    }

    @property auto front() const pure nothrow @nogc
    {
        return cast(KeyValueTemplate!(K,V))current.keyValue;
    }

    void popFront() pure nothrow @nogc
    {
        if (current.next)
            current = current.next;
        else
        {
            bIndex++;
            toNext();
        }
    }

    private void toNext() pure nothrow @nogc
    {
        for (; bIndex < aa.b_length; bIndex++)
        {
            if (auto next = aa.b[bIndex])
            {
                current = next;
                return;
            }
        }
        current = null;
    }
}

unittest
{
    AA* aa = null;
    foreach(keyValue; aa.asRange)
        assert(0);

    enum totalKeyLength = 50;
    foreach (i; 1 .. totalKeyLength + 1)
    {
        auto key = cast(void*)i;
        {
            auto valuePtr = dmd_aaGet(&aa, key);
            assert(valuePtr);
            *valuePtr = key;
        }
        bool[totalKeyLength] found;
        size_t rangeCount = 0;
        foreach (keyValue; aa.asRange)
        {
            assert(keyValue.key <= key);
            assert(keyValue.key == keyValue.value);
            rangeCount++;
            assert(!found[cast(size_t)keyValue.key - 1]);
            found[cast(size_t)keyValue.key - 1] = true;
        }
        assert(rangeCount == i);
    }
}

/********************************************
 * Rehash an array.
 */
private void dmd_aaRehash(AA** paa) pure nothrow
{
    //printf("Rehash\n");
    if (*paa)
    {
        AA* aa = *paa;
        if (aa)
        {
            size_t len = aa.b_length;
            if (len == 4)
                len = 32;
            else
                len *= 4;
            aaA** newb = cast(aaA**)mem.xmalloc(aaA.sizeof * len);
            memset(newb, 0, len * (aaA*).sizeof);
            for (size_t k = 0; k < aa.b_length; k++)
            {
                aaA* e = aa.b[k];
                while (e)
                {
                    aaA* enext = e.next;
                    size_t j = hash(cast(size_t)e.key) & (len - 1);
                    e.next = newb[j];
                    newb[j] = e;
                    e = enext;
                }
            }
            if (aa.b != cast(aaA**)aa.binit)
                mem.xfree(aa.b);
            aa.b = newb;
            aa.b_length = len;
        }
    }
}

unittest
{
    AA* aa = null;
    Value v = dmd_aaGetRvalue(aa, null);
    assert(!v);
    Value* pv = dmd_aaGet(&aa, null);
    assert(pv);
    *pv = cast(void*)3;
    v = dmd_aaGetRvalue(aa, null);
    assert(v == cast(void*)3);
}

struct AssocArray(K,V)
{
    private AA* aa;

    /**
    Returns: The number of key/value pairs.
    */
    @property size_t length() const pure nothrow @nogc @safe
    {
        return dmd_aaLen(aa);
    }

    /**
    Lookup value associated with `key` and return the address to it. If the `key`
    has not been added, it adds it and returns the address to the new value.

    Params:
        key = key to lookup the value for

    Returns: the address to the value associated with `key`. If `key` does not exist, it
             is added and the address to the new value is returned.
    */
    V* getLvalue(const(K) key) pure nothrow
    {
        return cast(V*)dmd_aaGet(&aa, cast(void*)key);
    }

    /**
    Lookup and return the value associated with `key`, if the `key` has not been
    added, it returns null.

    Params:
        key = key to lookup the value for

    Returns: the value associated with `key` if present, otherwise, null.
    */
    V opIndex(const(K) key) pure nothrow @nogc
    {
        return cast(V)dmd_aaGetRvalue(aa, cast(void*)key);
    }

    /**
    Gets a range of key/values for `aa`.

    Returns: a range of key/values for `aa`.
    */
    @property auto asRange() pure nothrow @nogc
    {
        return AARange!(K,V)(aa);
    }
}

///
unittest
{
    auto foo = new Object();
    auto bar = new Object();

    AssocArray!(Object, Object) aa;

    assert(aa[foo] is null);
    assert(aa.length == 0);

    auto fooValuePtr = aa.getLvalue(foo);
    *fooValuePtr = bar;

    assert(aa[foo] is bar);
    assert(aa.length == 1);
}
