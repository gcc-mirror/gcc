/* { dg-do link } */
/* { dg-do run { target { vma_equals_lma } } } */
/* { dg-require-effective-target persistent } */
/* { dg-skip-if "data LMA != VMA" { msp430-*-* } { "-mlarge" } } */
/* { dg-options "-save-temps" } */
/* { dg-final { scan-assembler ".section\t.persistent,\"aw\"\n" } } */

/* Test the "persistent" attribute.  */
#include "attr-persistent-main.inc"
