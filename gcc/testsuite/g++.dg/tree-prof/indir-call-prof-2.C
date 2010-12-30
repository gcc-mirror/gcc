/* { dg-options "-O" } */

int foo1(void) { return 0; }
int bar1(void) { throw 1; }
void foo2(void) { }
void bar2(void) { throw 1; }
void __attribute__((noinline,noclone)) test1(void (*f)(void)) { (*f)(); }
void __attribute__((noinline,noclone)) test2(void (*f)(void)) { (*f)(); }
int __attribute__((noinline,noclone)) test3(int (*f)(void)) { return (*f)(); }
int __attribute__((noinline,noclone)) test4(int (*f)(void)) { return (*f)(); }
int __attribute__((noinline,noclone)) test5(int (*f)(void), int x) { return x ? x : (*f)(); }
int __attribute__((noinline,noclone)) test6(int (*f)(void), int x) { return x ? x : (*f)(); }
void __attribute__((noinline,noclone)) test7(void (*f)(void)) { try { (*f)(); } catch (...) {} }
void __attribute__((noinline,noclone)) test8(void (*f)(void)) { try { (*f)();  } catch (...) {}}
int __attribute__((noinline,noclone)) test9(int (*f)(void)) { try { return (*f)(); } catch (...) {return 0;} }
int __attribute__((noinline,noclone)) test10(int (*f)(void)) { try { return (*f)(); } catch (...) {return 0;} }
int __attribute__((noinline,noclone)) test11(int (*f)(void), int x) { try { return x ? x : (*f)(); } catch (...) {return 0;} }
int __attribute__((noinline,noclone)) test12(int (*f)(void), int x) { try { return x ? x : (*f)(); } catch (...) {return 0;} }

int main()
{
  for (int i = 0; i < 100; ++i) test1(foo2);
  for (int i = 0; i < 100; ++i) try { test2(bar2); } catch (...) {} 
  for (int i = 0; i < 100; ++i) test3(foo1);
  for (int i = 0; i < 100; ++i) try { test4(bar1); } catch (...) {} 
  for (int i = 0; i < 100; ++i) test5(foo1, 0);
  for (int i = 0; i < 100; ++i) try { test6(bar1, 0); } catch (...) {} 
  for (int i = 0; i < 100; ++i) test7(foo2);
  for (int i = 0; i < 100; ++i) try { test8(bar2); } catch (...) {} 
  for (int i = 0; i < 100; ++i) test9(foo1);
  for (int i = 0; i < 100; ++i) try { test10(bar1); } catch (...) {} 
  for (int i = 0; i < 100; ++i) test11(foo1, 0);
  for (int i = 0; i < 100; ++i) try { test12(bar1, 0); } catch (...) {} 
  return 0;
}
