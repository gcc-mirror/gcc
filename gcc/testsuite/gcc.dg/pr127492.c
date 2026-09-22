/* PR middle-end/127492 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-latomic" { target libatomic_available } } */

#ifdef __SIZEOF_INT128__
#define T __int128
#else
#define T long long
#endif
T v = 1;

int
main ()
{
  T r = __atomic_add_fetch (&v, v, __ATOMIC_RELAXED);
  if (r != 2 || v != 2)
    __builtin_abort ();
}
