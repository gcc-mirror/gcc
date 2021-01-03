// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=97843
// { dg-additional-options "-fmain -funittest" }
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct Sdtor
{
    int value;
    ~this() { }
}

Sdtor sum(Sdtor[] sdtors)
{
    int result;
    foreach (s; sdtors)
        result += s.value;
    return Sdtor(result);
}

uint sum(uint[] ints)
{
    uint result;
    foreach(i; ints)
        result += i;
    return result;
}

unittest
{
    Sdtor[] sdtors = [Sdtor(0), Sdtor(1)];
    sdtors ~= sum(sdtors);
    assert(sdtors == [Sdtor(0), Sdtor(1), Sdtor(1)]);

    uint[] ints = [0, 1];
    ints ~= ints.sum;
    assert(ints == [0, 1, 1]);
}
