/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-additional-options "-mno-sse" { target x86_64-*-* i?86-*-* } } */

typedef int c __attribute__ ((__vector_size__ (16)));

__attribute__((always_inline)) void d (void); /* { dg-warning "" } { dg-error 7 "inlining failed" } */
void f(c x);

inline void e (c *b) {
    f (*b);
}

void a ()
{
  struct
    {
      c g[4];
    } h;
  d ();
  e (h.g);
}

