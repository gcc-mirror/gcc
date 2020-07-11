// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=94777
// { dg-do compile }

extern void variadic(...);

void f94777()
{
    struct S94777
    {
        int fld;
        this(this) { }
    }
    auto var = S94777(0);
    variadic(var, S94777(1));
}
