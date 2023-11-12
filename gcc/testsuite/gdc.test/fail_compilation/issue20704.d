/* TEST_OUTPUT:
---
fail_compilation/issue20704.d(17): Error: cannot create default argument for `ref` / `out` parameter from constant `0`
fail_compilation/issue20704.d(28): Error: template instance `issue20704.f2!int` error instantiating
fail_compilation/issue20704.d(19): Error: cannot create default argument for `ref` / `out` parameter from constant `0`
fail_compilation/issue20704.d(30): Error: template instance `issue20704.f4!int` error instantiating
fail_compilation/issue20704.d(17): Error: cannot create default argument for `ref` / `out` parameter from expression `S(0)` because it is not an lvalue
fail_compilation/issue20704.d(36): Error: template instance `issue20704.f2!(S)` error instantiating
fail_compilation/issue20704.d(17): Error: cannot create default argument for `ref` / `out` parameter from expression `null` because it is not an lvalue
fail_compilation/issue20704.d(38): Error: template instance `issue20704.f2!(C)` error instantiating
---
*/

// https://issues.dlang.org/show_bug.cgi?id=20704

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
