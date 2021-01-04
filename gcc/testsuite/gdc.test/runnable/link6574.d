// PERMUTE_ARGS:
module link6574;

import imports.testmangle;

enum Method { A, B, }

int foo(Method method = Method.A)()
{
    static assert(foo.mangleof == "_D8link6574"~tl!"28"~"__T3fooVE"~id!("8link6574","Qs")~"6Methodi0Z"~id!("3foo","Qs")~"FZi");
    return 10 * foo!method();
}
int foo(Method method : Method.A)()
{
    static assert(foo.mangleof == "_D8link6574"~tl!"29"~"__T3fooHVE"~id!("8link6574","Qt")~"6Methodi0Z"~id!("3foo","Qt")~"FZi");
    return 2;
}
int foo(Method method : Method.B)()
{
    static assert(0);
    return 3;
}

int bar(Method method = Method.B)()
{
    static assert(bar.mangleof == "_D8link6574"~tl!"28"~"__T3barVE"~id!("8link6574","Qs")~"6Methodi1Z"~id!("3bar","Qs")~"FZi");
    return 10 * bar!method();
}
int bar(Method method : Method.A)()
{
    static assert(0);
    return 2;
}
int bar(Method method : Method.B)()
{
    static assert(bar.mangleof == "_D8link6574"~tl!"29"~"__T3barHVE"~id!("8link6574","Qt")~"6Methodi1Z"~id!("3bar","Qt")~"FZi");
    return 3;
}

void main()
{
    assert(foo!() == 10 * 2);
    assert(foo() == 10 * 2);

    assert(bar!() == 10 * 3);
    assert(bar() == 10 * 3);
}
