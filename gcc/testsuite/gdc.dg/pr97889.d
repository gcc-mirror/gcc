// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=97889
// { dg-additional-options "-fmain -funittest" }
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

auto cat11ret3(T)(ref T s)
{
    s ~= 11;
    return [3];
}

unittest
{
    static auto test1(int[] val) { val ~= cat11ret3(val); return val; }
    assert(test1([1]) == [1, 11, 3]);
    static assert(test1([1]) == [1, 11, 3]);

    static auto test2(int[] val) { val = val ~ cat11ret3(val); return val; }
    // FIXME: assert(test2([1]) == [1, 3]);
    static assert(test2([1]) == [1, 3]);

    static auto test3(int[] val) { (val ~= 7) ~= cat11ret3(val); return val; }
    assert(test3([2]) == [2, 7, 11, 3]);
    static assert(test3([2]) == [2, 7, 11, 3]);

    static auto test4(int[] val) { (val ~= cat11ret3(val)) ~= 7; return val; }
    assert(test4([2]) == [2, 11, 3, 7]);
    static assert(test4([2]) == [2, 11, 3, 7]);
}
