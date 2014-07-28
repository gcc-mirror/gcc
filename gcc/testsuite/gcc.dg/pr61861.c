/* { dg-do compile } */
/* { dg-prune-output "expected" } */

extern void foo (int);
extern void bar (int, char *);

#define F __FILE__ /* { dg-error "11:passing argument" } */
#define T __TIME__ /* { dg-error "11:passing argument" } */
#define D __DATE__ /* { dg-error "11:passing argument" } */
#define L __LINE__ /* { dg-error "11:passing argument" } */

#define F2 "foo" /* { dg-error "12:passing argument" } */
#define T2 "foo" /* { dg-error "12:passing argument" } */
#define D2 "foo" /* { dg-error "12:passing argument" } */
#define L2 42 /* { dg-error "12:passing argument" } */

void
f (void)
{
  foo (__FILE__); /* { dg-error "8:passing argument" } */
  foo (__BASE_FILE__); /* { dg-error "8:passing argument" } */
  foo (__TIME__); /* { dg-error "8:passing argument" } */
  foo (__DATE__); /* { dg-error "8:passing argument" } */
  foo (__TIMESTAMP__); /* { dg-error "8:passing argument" } */
  bar (1, __LINE__); /* { dg-error "11:passing argument" } */
  bar (__COUNTER__, __COUNTER__); /* { dg-error "21:passing argument" } */

  foo (F); /* { dg-message "8:in expansion of" } */
  foo (T); /* { dg-message "8:in expansion of" } */
  foo (D); /* { dg-message "8:in expansion of" } */
  bar (1, L); /* { dg-message "11:in expansion of" } */

  foo (F2); /* { dg-message "8:in expansion of" } */
  foo (T2); /* { dg-message "8:in expansion of" } */
  foo (D2); /* { dg-message "8:in expansion of" } */
  bar (1, L2); /* { dg-message "11:in expansion of" } */
}
