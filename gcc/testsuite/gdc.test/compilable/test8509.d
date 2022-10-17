module test8509;
enum E : string { a = "hello", b = "world" }
struct S
{
    E opBinary(string s : "~")(S s) { return E.a; }
    E opBinary(string s : "~")(string s) { return E.a; }
}

void main()
{
    E e3 = S() ~ S();
    E e4 = S() ~ "a";
    assert(e3 == E.a);
    assert(e4 == E.a);
}
