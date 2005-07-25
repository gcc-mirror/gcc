/* PR middle-end/20303 */
/* Test nesting of #pragma GCC visibility. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-not-hidden "foo00" } } */
/* { dg-final { scan-hidden "foo01" } } */
/* { dg-final { scan-not-hidden "foo02" } } */
/* { dg-final { scan-hidden "foo03" } } */
/* { dg-final { scan-not-hidden "foo04" } } */
/* { dg-final { scan-not-hidden "foo05" } } */
/* { dg-final { scan-not-hidden "foo06" } } */
/* { dg-final { scan-hidden "foo07" } } */
/* { dg-final { scan-not-hidden "foo08" } } */
/* { dg-final { scan-hidden "foo09" } } */
/* { dg-final { scan-not-hidden "foo10" } } */
/* { dg-final { scan-hidden "foo11" } } */
/* { dg-final { scan-hidden "foo12" } } */
/* { dg-final { scan-hidden "foo13" } } */
/* { dg-final { scan-not-hidden "foo14" } } */
/* { dg-final { scan-hidden "foo15" } } */
/* { dg-final { scan-not-hidden "foo16" } } */
/* { dg-final { scan-hidden "foo17" } } */
/* { dg-final { scan-not-hidden "foo18" } } */
/* { dg-final { scan-hidden "foo19" } } */
/* { dg-final { scan-not-hidden "foo20" } } */
/* { dg-final { scan-hidden "foo21" } } */
/* { dg-final { scan-not-hidden "foo22" } } */
/* { dg-final { scan-hidden "foo23" } } */
/* { dg-final { scan-not-hidden "foo24" } } */
/* { dg-final { scan-hidden "foo25" } } */
/* { dg-final { scan-not-hidden "foo26" } } */
/* { dg-final { scan-hidden "foo27" } } */
/* { dg-final { scan-not-hidden "foo28" } } */
/* { dg-final { scan-hidden "foo29" } } */
/* { dg-final { scan-not-hidden "foo30" } } */
/* { dg-final { scan-hidden "foo31" } } */
/* { dg-final { scan-not-hidden "foo32" } } */
/* { dg-final { scan-hidden "foo33" } } */
/* { dg-final { scan-not-hidden "foo34" } } */
/* { dg-final { scan-hidden "foo35" } } */
/* { dg-final { scan-not-hidden "foo36" } } */
/* { dg-final { scan-hidden "foo37" } } */
/* { dg-final { scan-not-hidden "foo38" } } */
/* { dg-final { scan-hidden "foo39" } } */
/* { dg-final { scan-not-hidden "foo40" } } */
/* { dg-final { scan-hidden "foo41" } } */
/* { dg-final { scan-not-hidden "foo42" } } */
/* { dg-final { scan-hidden "foo43" } } */
/* { dg-final { scan-not-hidden "foo44" } } */
/* { dg-final { scan-hidden "foo45" } } */
/* { dg-final { scan-hidden "foo46" } } */
/* { dg-final { scan-hidden "foo47" } } */
/* { dg-final { scan-not-hidden "foo48" } } */
/* { dg-final { scan-hidden "foo49" } } */
/* { dg-final { scan-not-hidden "foo50" } } */
/* { dg-final { scan-hidden "foo51" } } */
/* { dg-final { scan-not-hidden "foo52" } } */
/* { dg-final { scan-not-hidden "foo53" } } */
/* { dg-final { scan-not-hidden "foo54" } } */
/* { dg-final { scan-hidden "foo55" } } */
/* { dg-final { scan-not-hidden "foo56" } } */
/* { dg-final { scan-hidden "foo57" } } */
/* { dg-final { scan-not-hidden "foo58" } } */
/* { dg-final { scan-hidden "foo59" } } */

#pragma GCC visibility push(default)
void foo00();
#pragma GCC visibility push(hidden)
void foo01();
#pragma GCC visibility push(default)
void foo02();
#pragma GCC visibility push(hidden)
void foo03();
#pragma GCC visibility push(default)
void foo04();
#pragma GCC visibility push(default)
void foo05();
#pragma GCC visibility push(default)
void foo06();
#pragma GCC visibility push(hidden)
void foo07();
#pragma GCC visibility push(default)
void foo08();
#pragma GCC visibility push(hidden)
void foo09();
#pragma GCC visibility push(default)
void foo10();
#pragma GCC visibility push(hidden)
void foo11();
#pragma GCC visibility push(hidden)
void foo12();
#pragma GCC visibility push(hidden)
void foo13();
#pragma GCC visibility push(default)
void foo14();
#pragma GCC visibility push(hidden)
void foo15();
#pragma GCC visibility push(default)
void foo16();
#pragma GCC visibility push(hidden)
void foo17();
#pragma GCC visibility push(default)
void foo18();
#pragma GCC visibility push(hidden)
void foo19();
#pragma GCC visibility push(default)
void foo20();
#pragma GCC visibility push(hidden)
void foo21();
#pragma GCC visibility push(default)
void foo22();
#pragma GCC visibility push(hidden)
void foo23();
#pragma GCC visibility push(default)
void foo24();
#pragma GCC visibility push(hidden)
void foo25();
#pragma GCC visibility push(default)
void foo26();
#pragma GCC visibility push(hidden)
void foo27();
#pragma GCC visibility push(default)
void foo28();
#pragma GCC visibility push(hidden)
void foo29();
#pragma GCC visibility pop
void foo30();
#pragma GCC visibility pop
void foo31();
#pragma GCC visibility pop
void foo32();
#pragma GCC visibility pop
void foo33();
#pragma GCC visibility pop
void foo34();
#pragma GCC visibility pop
void foo35();
#pragma GCC visibility pop
void foo36();
#pragma GCC visibility pop
void foo37();
#pragma GCC visibility pop
void foo38();
#pragma GCC visibility pop
void foo39();
#pragma GCC visibility pop
void foo40();
#pragma GCC visibility pop
void foo41();
#pragma GCC visibility pop
void foo42();
#pragma GCC visibility pop
void foo43();
#pragma GCC visibility pop
void foo44();
#pragma GCC visibility pop
void foo45();
#pragma GCC visibility pop
void foo46();
#pragma GCC visibility pop
void foo47();
#pragma GCC visibility pop
void foo48();
#pragma GCC visibility pop
void foo49();
#pragma GCC visibility pop
void foo50();
#pragma GCC visibility pop
void foo51();
#pragma GCC visibility pop
void foo52();
#pragma GCC visibility pop
void foo53();
#pragma GCC visibility pop
void foo54();
#pragma GCC visibility pop
void foo55();
#pragma GCC visibility pop
void foo56();
#pragma GCC visibility pop
void foo57();
#pragma GCC visibility pop
void foo58();
#pragma GCC visibility push (hidden)
void foo59();
#pragma GCC visibility pop
#pragma GCC visibility pop

#define D(N) \
void foo##N##0() { } \
void foo##N##1() { } \
void foo##N##2() { } \
void foo##N##3() { } \
void foo##N##4() { } \
void foo##N##5() { } \
void foo##N##6() { } \
void foo##N##7() { } \
void foo##N##8() { } \
void foo##N##9() { }
D(0)
D(1)
D(2)
D(3)
D(4)
D(5)
