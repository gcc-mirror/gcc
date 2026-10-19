/* { dg-do compile { target bitint } } */
/* { dg-options "-O -fdump-tree-ccp1" } */

#if __BITINT_MAXWIDTH__ >= 129
typedef unsigned _BitInt(129) T;

T
foo ()
{
  T y = (T) 1;
  T x = __builtin_bitreverseg (y);
  return x;
}
#endif

/* { dg-final { scan-tree-dump-not "BITREVERSE" "ccp1" } } */
