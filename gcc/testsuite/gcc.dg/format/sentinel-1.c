/* Test for attribute sentinel.  */
/* Origin: Kaveh Ghazi <ghazi@caip.rutgers.edu> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

#include <stddef.h> /* For NULL, which must be (ptr)0.  */

extern int execl (const char *, const char *, ...);
extern int execlp (const char *, const char *, ...);

#define ATTR __attribute__ ((__sentinel__))

extern int a ATTR; /* { dg-warning "applies to function types" "sentinel" } */

extern void foo1 (const char *, ...) ATTR;
extern void foo2 (...) ATTR; /* { dg-error "ISO C requires|named arguments" "sentinel" } */
extern void foo3 () ATTR; /* { dg-warning "named arguments" "sentinel" } */
extern void foo4 (const char *, int) ATTR; /* { dg-warning "variadic functions" "sentinel" } */

extern void bar(void)
{
  foo1 (); /* { dg-error "missing sentinel|too few arguments" "sentinel" } */
  foo1 ("a"); /* { dg-warning "missing sentinel" "sentinel" } */
  foo1 ("a", 1); /* { dg-warning "missing sentinel" "sentinel" } */
  foo1 ("a", 0); /* { dg-warning "missing sentinel" "sentinel" } */
  foo1 ("a", (void*)1); /* { dg-warning "missing sentinel" "sentinel" } */
  foo1 ("a", NULL, 1); /* { dg-warning "missing sentinel" "sentinel" } */
  foo1 ("a", NULL);

  execl ("/bin/ls", "-aFC"); /* { dg-warning "missing sentinel" "sentinel" } */
  execl ("/bin/ls", "-aFC", 0); /* { dg-warning "missing sentinel" "sentinel" } */
  execl ("/bin/ls", "-aFC", NULL);

  execlp ("ls", "-aFC"); /* { dg-warning "missing sentinel" "sentinel" } */
  execlp ("ls", "-aFC", 0); /* { dg-warning "missing sentinel" "sentinel" } */
  execlp ("ls", "-aFC", NULL);
}
