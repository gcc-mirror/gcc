/* PR c/128184 */
/* { dg-do run { target { bitint && libatomic_available } } } */
/* { dg-options "-O2 -latomic" } */

#if __BITINT_MAXWIDTH__ >= 192
_BitInt(192) a;
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 192
  __atomic_fetch_nand (&a, 5, __ATOMIC_RELAXED);
  if (a != -1)
    __builtin_abort ();
#endif
}
