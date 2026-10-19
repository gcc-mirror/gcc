/* PR middle-end/127492 */
/* { dg-do run { target { int128 && { sync_int_128_runtime || libatomic_available } } } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-latomic" { target libatomic_available } } */

#ifndef T
#define T __int128
#endif
T v = 1;

int
main ()
{
  T r = __atomic_add_fetch (&v, v, __ATOMIC_RELAXED);
  if (r != 2 || v != 2)
    __builtin_abort ();
}
