module test8509;
enum E : string { a = "hello", b = "world" }
struct S { E opCat(S s) { return E.a; } E opCat(string s) { return E.a; } }

void main()
{
    E e3 = S() ~ S();
    E e4 = S() ~ "a";
    assert(e3 == E.a);
    assert(e4 == E.a);
}
