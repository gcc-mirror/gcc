/* { dg-do compile } */
/* { dg-options "-Wall" } */
/* { dg-error "" "" { target *-*-solaris2.5.1 mmix-*-* mips*-*-elf* } 0 } */

/* Compile with -Wall to get a warning if built-in and system intmax_t don't
   match.  */

#include <inttypes.h>

__INTMAX_TYPE__ __im_t__;
__UINTMAX_TYPE__ __uim_t__;
intmax_t *im_t_p;
uintmax_t *uim_t_p;

void
imt (void)
{
  im_t_p = &__im_t__;
}

void
uimt (void)
{
  uim_t_p = &__uim_t__;
}
