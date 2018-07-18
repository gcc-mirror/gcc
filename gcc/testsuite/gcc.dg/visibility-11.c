/* PR middle-end/20297 */
/* The memcpy FUNCTION_DECL built in the middle-end for block moves got
   hidden visibility from the first push, so the call didn't use the PLT.  */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-skip-if "" { *-*-darwin* } } */
/* { dg-require-visibility "" } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-Os -fpic -mstringop-strategy=libcall" } */
/* { dg-final { scan-assembler "memcpy@PLT" } } */

#pragma GCC visibility push(hidden)
#pragma GCC visibility push(default)
extern void* memcpy (void *, const void *, __SIZE_TYPE__);
#pragma GCC visibility pop

struct a { int a[4096]; };

extern void *bar (struct a *, struct a *, int);

void *
foo (struct a *a, struct a *b, int c)
{
  struct a cc = *b;
  return bar (a, &cc, 4 * c);
}
