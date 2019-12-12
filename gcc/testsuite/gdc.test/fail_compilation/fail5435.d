// 5435

template Tuple5435(E...) { alias E Tuple5435; }
enum Enum5435 { A, B, C };

void main()
{
    alias Tuple5435!(Enum5435.A, Enum5435.B, Enum5435.C, "foo", 3.0) tup;

    foreach (Enum5435 foo; tup) pragma(msg, foo);
    foreach (  string foo; tup) pragma(msg, foo);
    foreach (     int foo; tup) pragma(msg, foo);
}
