/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -Wno-psabi" } */

/* Same as pr128921-1.c, but for the return value, which must be returned
   through a hidden pointer in %rdi.  */

struct B { int b : 5; };
struct __attribute__((packed)) S { char a; struct B s; };

struct S
g (void)
{
  struct S s = { 7, { -5 } };
  return s;
}

/* { dg-final { scan-assembler "\\(%\[er\]di\\)" } } */
