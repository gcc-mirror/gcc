// https://issues.dlang.org/show_bug.cgi?id=23351
enum strings =
[
"a[(b).",
"[(a)(b).",
"a(={@.()(",
"a[b,[(c).",
"a[b#([(c).",
"[a@b[(c).",
"[((a).",
"[a)b[(c).",
"a[b)[(c).",
"a(b[(c).",
"a[b()c[(d).",
"a[(b[(c).",
"a(b[(c).",
"[(@@a b[(c).",
"a[(!b)c[(d).",
"[(^a)b[(c).",
"a(b[(c).",
"~[a.b[(c).",
"[a).[(b c d(e[(f).",
"[((a).",
"[a}b[(c).",
"a[b[c..(d).",
"[1a.[(b).",
"a[({in){,",
"a[^in(b[c=])S....,",
"a[({in[({)){,"
];
template KidNamedFinger(T)
{

}
void dummy()
{
    static foreach(str; strings)
    {
        /*
            The above strings are all gibberish, they should
            fail to parse but not segfault the compiler.
        */
        {
            enum exp = __traits(compiles, mixin(str));
            static assert(!exp);
            enum t = __traits(compiles, KidNamedFinger!(mixin(str)));
            static assert(!t);
        }
    }
}
