/* { dg-do compile } */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mcmodel=large" } */

/* PR target/120681 -- verify that -mcpu=power10 -mcmodel=large uses PC
   relative addressing instead of using TOC addressing.  */

#ifndef TYPE
#define TYPE unsigned long
#endif

extern TYPE global_var;

void
set_global (TYPE value)
{
  /*
   * Generate:
   * pld 9,global_var@got@pcrel
   * std 3,0(9)
   *
   * Not:
   * addis 9,2,.LC0@toc@ha
   * ld    9,.LC0@toc@l(9)
   * std   3,0(9)
   */

  global_var = value;
}

/* { dg-final { scan-assembler     {@got@pcrel} } } */
/* { dg-final { scan-assembler-not {@toc@ha}    } } */
/* { dg-final { scan-assembler-not {@toc@l}     } } */
