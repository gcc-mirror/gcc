/* PR debug/114015 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-g -fvar-tracking-assignments -fdebug-types-section -w" } */

#if __BITINT_MAXWIDTH__ >= 236
typedef _BitInt(236) B;
#else
typedef _BitInt(63) B;
#endif

int
foo (B n, struct { char a[n]; } o)
{
}
