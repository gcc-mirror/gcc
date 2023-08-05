/* PR target/110717 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#ifdef __SIZEOF_INT128__
#define type __int128
#define N 59
#else
#define type long long
#define N 27
#endif

struct S { type a : sizeof (type) * __CHAR_BIT__ - N; };

unsigned type bar (struct S *p)
{
  return p->a;
}

/* { dg-final { scan-assembler-not "andl" } } */
