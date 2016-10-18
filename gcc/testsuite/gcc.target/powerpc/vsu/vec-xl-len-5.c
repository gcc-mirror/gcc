/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

#include <stddef.h>
#include <altivec.h>

__vector unsigned __int128
fetch_data (unsigned __int128 *address, size_t length)
{
  return vec_xl_len (address, length);
}

/* { dg-final { scan-assembler "sldi" } } */
/* { dg-final { scan-assembler "lxvl" } } */
