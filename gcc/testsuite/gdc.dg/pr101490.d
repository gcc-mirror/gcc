// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=101490
// { dg-do compile }

struct S101490
{
    int[0] arr;
}

void main()
{
    S101490* t;
    auto a = cast(typeof(t.arr)[0])t.arr;
    write(a);
}

void write(S)(S args)
{
    foreach (arg; args)
    {
    }
}
