import std.stdio;
import std.algorithm;

void test1()
{
    int[] a = [1,2,3,4,5];
    writeln( remove!("a < 3")(a) );
}

void test2()
{
    auto arr = [1,2,3,4,5];
    auto m = map!"a + 1"(filter!"a < 4"(arr));
}

void main()
{
    test1();
    test2();

    writeln("Success");
}
