module test8509;
enum E : string { a = "hello", b = "world" }

void main()
{
    E e1 = E.a ~ " world";
    E e2 = "hello " ~ E.b;
}
