/* PR c/80076  */
/* { dg-options "-Wmisleading-indentation" }  */

void foo(void);

void test01(int flag) {
#define bar() foo() /* { dg-message "this statement" }  */
  if (flag) /* { dg-warning "does not guard" }  */
    foo();
    bar(); /* { dg-message "in expansion of macro" }  */
#undef bar
}

void test02(int flag) {
#define bar() foo()
  if (flag) /* { dg-warning "does not guard" }  */
    bar();
    foo(); /* { dg-message "this statement" }  */
#undef bar
}

void test03(int flag) {
#define bar() foo() /* { dg-message "this statement" }  */
  if (flag) /* { dg-warning "does not guard" }  */
    bar();
    bar(); /* { dg-message "in expansion of macro" }  */
#undef bar
}

void test04(int flag, int num) {
#define bar() \
  {		\
    if (flag)	\
      num = 0;	\
      num = 1;	\
  }
  bar();
/* { dg-warning "does not guard" "" { target *-*-* } .-5 }  */
/* { dg-message "this statement" "" { target *-*-* } .-4 }  */
#undef bar
}

void test05(int flag, int num) {
#define baz() (num = 1)
#define bar() \
  {		\
    if (flag)	\
      num = 0;	\
      baz();	\
  }
#define wrapper bar
  wrapper();
/* { dg-warning "does not guard" "" { target *-*-* } .-6 }  */
/* { dg-message "this statement" "" { target *-*-* } .-10 }  */
#undef bar
}
