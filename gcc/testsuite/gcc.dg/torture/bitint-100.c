/* PR target/124948 */
/* { dg-do run { target { bitint && sync_int_long } } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

_BitInt(17) a;
unsigned _BitInt(17) b;

#include "../bitintext.h"

[[gnu::noipa]] _BitInt(17)
f1 (_BitInt(17) *p, _BitInt(32) q)
{
  return __atomic_add_fetch (p, q, __ATOMIC_RELAXED);
}

[[gnu::noipa]] _BitInt(17)
f2 (_BitInt(17) *p, _BitInt(32) q)
{
  return __atomic_sub_fetch (p, q, __ATOMIC_RELAXED);
}

[[gnu::noipa]] _BitInt(17)
f3 (_BitInt(17) *p, _BitInt(32) q)
{
  return __atomic_fetch_add (p, q, __ATOMIC_RELAXED);
}

[[gnu::noipa]] _BitInt(17)
f4 (_BitInt(17) *p, _BitInt(32) q)
{
  return __atomic_fetch_sub (p, q, __ATOMIC_RELAXED);
}

[[gnu::noipa]] _BitInt(17)
f5 (_BitInt(17) *p, _BitInt(32) q)
{
  return __sync_add_and_fetch (p, q);
}

[[gnu::noipa]] _BitInt(17)
f6 (_BitInt(17) *p, _BitInt(32) q)
{
  return __sync_sub_and_fetch (p, q);
}

[[gnu::noipa]] _BitInt(17)
f7 (_BitInt(17) *p, _BitInt(32) q)
{
  return __sync_fetch_and_add (p, q);
}

[[gnu::noipa]] _BitInt(17)
f8 (_BitInt(17) *p, _BitInt(32) q)
{
  return __sync_fetch_and_sub (p, q);
}

[[gnu::noipa]] unsigned _BitInt(17)
f9 (unsigned _BitInt(17) *p, unsigned _BitInt(32) q)
{
  return __atomic_add_fetch (p, q, __ATOMIC_RELAXED);
}

[[gnu::noipa]] unsigned _BitInt(17)
f10 (unsigned _BitInt(17) *p, unsigned _BitInt(32) q)
{
  return __atomic_sub_fetch (p, q, __ATOMIC_RELAXED);
}

[[gnu::noipa]] unsigned _BitInt(17)
f11 (unsigned _BitInt(17) *p, unsigned _BitInt(32) q)
{
  return __atomic_fetch_add (p, q, __ATOMIC_RELAXED);
}

[[gnu::noipa]] unsigned _BitInt(17)
f12 (unsigned _BitInt(17) *p, unsigned _BitInt(32) q)
{
  return __atomic_fetch_sub (p, q, __ATOMIC_RELAXED);
}

[[gnu::noipa]] unsigned _BitInt(17)
f13 (unsigned _BitInt(17) *p, unsigned _BitInt(32) q)
{
  return __sync_add_and_fetch (p, q);
}

[[gnu::noipa]] unsigned _BitInt(17)
f14 (unsigned _BitInt(17) *p, unsigned _BitInt(32) q)
{
  return __sync_sub_and_fetch (p, q);
}

[[gnu::noipa]] unsigned _BitInt(17)
f15 (unsigned _BitInt(17) *p, unsigned _BitInt(32) q)
{
  return __sync_fetch_and_add (p, q);
}

[[gnu::noipa]] unsigned _BitInt(17)
f16 (unsigned _BitInt(17) *p, unsigned _BitInt(32) q)
{
  return __sync_fetch_and_sub (p, q);
}

int
main ()
{
  _BitInt(17) c;
  unsigned _BitInt(17) d;
  a = 64295wb;
  BEXTC (a);
  c = f1 (&a, 187040987wb);
  BEXTC (a);
  BEXTC (c);
  if (a != -65534wb || c != a)
    __builtin_abort ();
  c = f2 (&a, 394264674wb);
  BEXTC (a);
  BEXTC (c);
  if (a != 65440wb || c != a)
    __builtin_abort ();
  c = f3 (&a, 840434595wb);
  BEXTC (a);
  BEXTC (c);
  if (a != -64701wb || c != 65440wb)
    __builtin_abort ();
  c = f4 (&a, 591403122wb);
  BEXTC (a);
  BEXTC (c);
  if (a != 60113wb || c != -64701wb)
    __builtin_abort ();
  c = f5 (&a, 1215571163wb);
  BEXTC (a);
  BEXTC (c);
  if (a != -61524wb || c != a)
    __builtin_abort ();
  c = f6 (&a, 1913664021wb);
  BEXTC (a);
  BEXTC (c);
  if (a != 56727wb || c != a)
    __builtin_abort ();
  c = f7 (&a, 858931586wb);
  BEXTC (a);
  BEXTC (c);
  if (a != -57575wb || c != 56727wb)
    __builtin_abort ();
  c = f8 (&a, 286413601wb);
  BEXTC (a);
  BEXTC (c);
  if (a != 52216wb || c != -57575wb)
    __builtin_abort ();
  b = 77593uwb;
  BEXTC (b);
  d = f9 (&b, 858861337uwb);
  BEXTC (b);
  BEXTC (d);
  if (b != 24114uwb || d != b)
    __builtin_abort ();
  d = f10 (&b, 1431383831uwb);
  BEXTC (b);
  BEXTC (d);
  if (b != 77595uwb || d != b)
    __builtin_abort ();
  d = f11 (&b, 305231735uwb);
  BEXTC (b);
  BEXTC (d);
  if (b != 42642uwb || d != 77595uwb)
    __builtin_abort ();
  d = f12 (&b, 591497352uwb);
  BEXTC (b);
  BEXTC (d);
  if (b != 73226uwb || d != 42642uwb)
    __builtin_abort ();
  d = f13 (&b, 1985058934uwb);
  BEXTC (b);
  BEXTC (d);
  if (b != 46720uwb || d != b)
    __builtin_abort ();
  d = f14 (&b, 840018893uwb);
  BEXTC (b);
  BEXTC (d);
  if (b != 68275uwb || d != b)
    __builtin_abort ();
  d = f15 (&b, 1698807006uwb);
  BEXTC (b);
  BEXTC (d);
  if (b != 51089uwb || d != 68275uwb)
    __builtin_abort ();
  d = f16 (&b, 877788892uwb);
  BEXTC (b);
  BEXTC (d);
  if (b != 51381uwb || d != 51089uwb)
    __builtin_abort ();
}
