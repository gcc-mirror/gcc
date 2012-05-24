/*
  { dg-options "-fshow-column -ftrack-macro-expansion" }
  { dg-do compile }
 */

#define PASTED var ## iable /* { dg-error "'variable' undeclared" } */
#define call_foo(p1, p2) \
  foo (p1,		 \
       p2);  /*  { dg-message "in definition of macro 'call_foo'" } */

void foo(int, char);

void
bar()
{
  call_foo(1,PASTED); /* { dg-message "in expansion of macro 'PASTED'" } */
}

