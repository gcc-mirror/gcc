/* Test for attribute sentinel.  */
/* Origin: Kaveh Ghazi <ghazi@caip.rutgers.edu> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

#include <stddef.h> /* For NULL, which must be (ptr)0.  */

extern int execl (const char *, const char *, ...);
extern int execlp (const char *, const char *, ...);
extern int execle (const char *, const char *, ...);
extern char *envp[];

#define ATTR __attribute__ ((__sentinel__))

extern int a ATTR; /* { dg-warning "applies to function types" "sentinel" } */

extern void foo1 (const char *, ...) ATTR; /* { dg-message "note: declared here" } */
extern void foo2 (...) ATTR; /* { dg-error "ISO C requires|named arguments" "sentinel" } */
extern void foo3 () ATTR; /* { dg-warning "named arguments" "sentinel" } */
extern void foo4 (const char *, int) ATTR; /* { dg-warning "variadic functions" "sentinel" } */
extern void foo5 (const char *, ...) __attribute__ ((__sentinel__(1)));
extern void foo6 (const char *, ...) __attribute__ ((__sentinel__(5)));
extern void foo7 (const char *, ...) __attribute__ ((__sentinel__(0)));
extern void foo8 (const char *, ...) __attribute__ ((__sentinel__("a"))); /* { dg-warning "not an integer constant" "sentinel" } */
extern void foo9 (const char *, ...) __attribute__ ((__sentinel__(-1))); /* { dg-warning "less than zero" "sentinel" } */
extern void foo10 (const char *, ...) __attribute__ ((__sentinel__(1,3))); /* { dg-error "wrong number of arguments" "sentinel" } */

extern void bar(void)
{
  foo1 (); /* { dg-error "not enough|too few arguments" "sentinel" } */
  foo1 (NULL); /* { dg-warning "not enough" "sentinel" } */
  foo1 ("a"); /* { dg-warning "not enough" "sentinel" } */
  foo1 ("a", 1); /* { dg-warning "missing sentinel" "sentinel" } */
  foo1 ("a", 0); /* { dg-warning "missing sentinel" "sentinel" } */
  foo1 ("a", (void*)1); /* { dg-warning "missing sentinel" "sentinel" } */
  foo1 ("a", NULL, 1); /* { dg-warning "missing sentinel" "sentinel" } */
  foo1 ("a", NULL);

  foo5 (NULL); /* { dg-warning "not enough" "sentinel" } */
  foo5 (NULL, 1); /* { dg-warning "not enough" "sentinel" } */
  foo5 ("a", NULL); /* { dg-warning "not enough" "sentinel" } */
  foo5 ("a", NULL, 1);
  foo5 ("a", 1, 2, 3, NULL); /* { dg-warning "missing sentinel" "sentinel" } */
  foo5 ("a", 1, 2, NULL, 3);
  foo5 ("a", 1, NULL, 2, 3); /* { dg-warning "missing sentinel" "sentinel" } */
  foo5 ("a", NULL, 1, 2, 3); /* { dg-warning "missing sentinel" "sentinel" } */
  foo5 ("a", 0, 1, 2, 3); /* { dg-warning "missing sentinel" "sentinel" } */

  foo6 ("a", 1, NULL); /* { dg-warning "not enough" "sentinel" } */
  foo6 ("a", 1, NULL, 2); /* { dg-warning "not enough" "sentinel" } */
  foo6 ("a", 1, NULL, 2, 3); /* { dg-warning "not enough" "sentinel" } */
  foo6 ("a", NULL, 1, 2, 3); /* { dg-warning "not enough" "sentinel" } */
  foo6 ("a", NULL, 1, 2, 3, 4); /* { dg-warning "not enough" "sentinel" } */
  foo6 (NULL, 1, 2, 3, 4, 5); /* { dg-warning "not enough" "sentinel" } */
  foo6 ("a", NULL, 1, 2, 3, 4, 5);
  foo6 ("a", 0, NULL, 1, 2, 3, 4, 5);
  foo6 ("a", 0, 1, 2, 3, 4, 5); /* { dg-warning "missing sentinel" "sentinel" } */
  foo6 ("a", NULL, 1, 2, 3, 4, 5, 6); /* { dg-warning "missing sentinel" "sentinel" } */

  foo7 ("a", 1, 2, 3, NULL);

  execl ("/bin/ls", "/bin/ls", "-aFC"); /* { dg-warning "missing sentinel" "sentinel" } */
  execl ("/bin/ls", "/bin/ls", "-aFC", 0); /* { dg-warning "missing sentinel" "sentinel" } */
  execl ("/bin/ls", "/bin/ls", "-aFC", NULL);

  execlp ("ls", "ls", "-aFC"); /* { dg-warning "missing sentinel" "sentinel" } */
  execlp ("ls", "ls", "-aFC", 0); /* { dg-warning "missing sentinel" "sentinel" } */
  execlp ("ls", "ls", "-aFC", NULL);

  execle ("ls", "ls", "-aFC", ".", envp); /* { dg-warning "missing sentinel" "sentinel" } */
  execle ("ls", "ls", "-aFC", ".", 0, envp); /* { dg-warning "missing sentinel" "sentinel" } */
  execle ("ls", "ls", "-aFC", ".", NULL, envp);
}
