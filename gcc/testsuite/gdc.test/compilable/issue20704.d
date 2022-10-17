/*
https://issues.dlang.org/show_bug.cgi?id=20704
REQUIRED_ARGS: -preview=rvaluerefparam
 */

void f1(T)(const auto ref T arg = T.init) {}
void f2(T)(const      ref T arg = T.init) {}
void f3(T)(const auto ref T arg = 0) {}
void f4(T)(const      ref T arg = 0) {}

struct S { int _; }
class C { int _; }

void main ()
{
    int i;
    f1!int(i);
    f2!int(i);
    f3!int(i);
    f4!int(i);
    f1!int();
    f2!int();
    f3!int();
    f4!int();
    f1!S();
    f2!S();
    f1!C();
    f2!C();
}
