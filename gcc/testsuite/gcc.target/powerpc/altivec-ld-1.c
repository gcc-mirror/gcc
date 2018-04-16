/* { dg-do run { target powerpc*-*-* } } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-maltivec -O0 -Wall -Wno-deprecated" } */

#include <altivec.h>
#include <stdlib.h>

static __vector __int128 v;
static __vector __int128 *pv;

static __vector unsigned __int128 uv;
static __vector unsigned __int128 *puv;

static __int128 i128;
static __int128 *pi128;

static unsigned __int128 u128;
static unsigned __int128 *pu128;


void
doInitialization ()
{
  v[0] = -1;
  pv = &v;

  uv[0] = 0xcafebabe;
  puv = &uv;

  i128 = 0xfabeabe;
  pi128 = &i128;

  u128 = 0xabefabe;
  pu128 = &u128;
}

int
main (int argc, char *argv[])
{
  __vector __int128 loaded_v;
  __vector unsigned __int128 loaded_uv;

  /* Usage:
   *  <Type> result = vec_ld (int index, __vector <Type> v)
   * is equivalent to:
   *  result = v [index];
   */
  doInitialization ();
  loaded_v = vec_ld (0, pv);
  if (loaded_v[0] != -1)
    abort ();

  loaded_uv = vec_ld (0, puv);
  if (loaded_uv[0] != 0xcafebabe)
    abort ();

  loaded_v = vec_ld (0, pi128);
  if (loaded_v[0] != 0xfabeabe)
    abort ();

  loaded_uv = vec_ld (0, pu128);
  if (loaded_uv[0] != 0xabefabe)
    abort ();

  return 0;
}
