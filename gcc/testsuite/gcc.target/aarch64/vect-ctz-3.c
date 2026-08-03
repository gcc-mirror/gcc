/* { dg-do run } */
/* { dg-options "-O3" } */

#include "vect-ctz.h"

#define N 61
static u8 a[N], d[N], e[N];
static u16 ah[N], dh[N], eh[N];

__attribute__((noipa, optimize ("O0"))) void
ctzb_ref (u8 *__restrict d, u8 *__restrict a)
{
  for (int i = 0; i < 16; i++)
    d[i] = __builtin_ctzg ((u8) (a[i] | 0x80));
}

__attribute__((noipa, optimize ("O0"))) void
ctzb_n_ref (u8 *__restrict d, u8 *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_ctzg (a[i], 8);
}

__attribute__((noipa, optimize ("O0"))) void
ctzh_ref (u16 *__restrict d, u16 *__restrict a)
{
  for (int i = 0; i < 8; i++)
    d[i] = __builtin_ctzg ((u16) (a[i] | 0x8000));
}

__attribute__((noipa, optimize ("O0"))) void
ctzh_n_ref (u16 *__restrict d, u16 *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_ctzg (a[i], 16);
}

int
main (void)
{
  for (int i = 0; i < N; i++)
    {
      a[i] = (u8) (i * 37 + (i & 7));
      ah[i] = (u16) (i * 9973 + (i & 15));
    }

  ctzb (d, a);
  ctzb_ref (e, a);
  for (int i = 0; i < 16; i++)
    if (d[i] != e[i])
      __builtin_abort ();

  ctzb_n (d, a, N);
  ctzb_n_ref (e, a, N);
  for (int i = 0; i < N; i++)
    if (d[i] != e[i])
      __builtin_abort ();

  ctzh (dh, ah);
  ctzh_ref (eh, ah);
  for (int i = 0; i < 8; i++)
    if (dh[i] != eh[i])
      __builtin_abort ();

  ctzh_n (dh, ah, N);
  ctzh_n_ref (eh, ah, N);
  for (int i = 0; i < N; i++)
    if (dh[i] != eh[i])
      __builtin_abort ();

  return 0;
}
