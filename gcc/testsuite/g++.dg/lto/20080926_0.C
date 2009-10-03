// { dg-lto-do assemble }
// { dg-lto-options {{-O2 -flto -fno-strict-aliasing}} }
extern int foo();
void bar() { try { int i = foo(); } catch(int) { } }
