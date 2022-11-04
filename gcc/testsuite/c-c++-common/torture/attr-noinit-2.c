/* { dg-do link } */
/* { dg-do run { target { vma_equals_lma } } } */
/* { dg-require-effective-target noinit } */
/* { dg-options "-fdata-sections -save-temps" } */
/* { dg-skip-if "data LMA != VMA" { msp430-*-* } { "-mlarge" } } */
/* { dg-final { scan-assembler ".section\t.noinit.var_noinit,\"aw\"\n" } } */

/* Test the "noinit" attribute with -fdata-sections.  */
#include "attr-noinit-main.inc"
