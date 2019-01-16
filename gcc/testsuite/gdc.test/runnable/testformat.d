// PERMUTE_ARGS:

import std.stdio;
import std.string;

/*************************************************************/

void test1()
{
    double x = 1e6;
    float f;
    double d;
    real r;

    while (x > 1e-5)
    {   double y = x / 101.0;

        std.stdio.writef("x = %g\t%f\t%e\n",x/101.0,x/101.0,x/101.0);
        x /= 10.0;
    }

    double a = 123456789.0;
    for (int i = 20; i--; a /= 10)
        std.stdio.writef("%20.6g|%20.6e|%20.6f\n",a,a,a);

    std.stdio.writef("%e %e %e %e\n",float.nan,double.nan,real.nan,double.infinity);
    std.stdio.writef("%e %e %e\n",-float.nan,-double.nan,-double.infinity);
    std.stdio.writef("%-5E %5E %E\n",float.nan,double.nan,double.infinity);

    std.stdio.writef("%f %f %f\n",float.nan,double.nan,double.infinity);
    std.stdio.writef("%f %f %f\n",-float.nan,-double.nan,-double.infinity);
    std.stdio.writef("%+F %+ F %F\n",float.nan,double.nan,double.infinity);

    std.stdio.writef("%g %g %g\n",float.nan,double.nan,double.infinity);
    std.stdio.writef("%g %g %g\n",-float.nan,-double.nan,-double.infinity);
    std.stdio.writef("% G %G %G\n",float.nan,double.nan,double.infinity);

    r = 0x1.AAp+3L;
    std.stdio.writef(" r = %g\n", r);
    std.stdio.writef(" r = %a\n", r);
    std.stdio.writef(" r = %A\n", r);

    d = 0x1.AAp+3;
    std.stdio.writef(" d = %.5a\n", d);
    std.stdio.writef(" d = %A\n", d);

    f = 0x1.AAp+3f;
    std.stdio.writef(" f = %a\n", f);
    std.stdio.writef(" f = %A\n", f);

    f = 0x1.FFp+3f;
    std.stdio.writef(" f = %.1a\n", f);
    std.stdio.writef(" f = %A\n", f);

    r = 0;
    std.stdio.writef(" r = %a\n", r);
    std.stdio.writef(" r = %A\n", r);

    std.stdio.writef("%e\n", 1e+300);
    std.stdio.writef("%.0f\n%.307f\n", 1e+300, 1e-300);
    std.stdio.writef("%.0A\n%.307A\n", 1e+300, 1e-300);
}

/*************************************************************/

void test2()
{
   writefln("%o", 9);
   assert(std.string.format("%o", 9) == "11");
   assert(std.string.format("%o", 10) == "12");
   assert(std.string.format("%b", 9) == "1001");
   assert(std.string.format("%b", 10) == "1010");
}


/*************************************************************/

void test3()
{
    Object e = new Exception("hello");
    writeln(e.toString());
    //assert(e.toString() == "object.Exception: hello");
    //assert(format(e) == "object.Exception: hello");

    alias char[] xstring;
    assert(format(cast(xstring)"world") == "world");
}

/*************************************************************/

void test4()
{
   const char[][] x = ["%s","123"];
   writeln(x);
   assert(std.string.format("%s", x) == `["%s", "123"]`);
}

/*************************************************************/

void test5()
{
    int[int] foo;

    foo[1] = 2;
    foo[7] = 28;

    writefln("%s", foo);

    void[0] v;
    assert(format("%s", v) == "[]");
}

/*************************************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();

    std.stdio.writefln("Success");
    return 0;
}
