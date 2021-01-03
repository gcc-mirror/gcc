/* { dg-do compile } */
/* { dg-require-effective-target persistent } */
/* { dg-skip-if "data LMA != VMA" { msp430-*-* } { "-mlarge" } } */
/* { dg-options "-fdata-sections -save-temps" } */
/* { dg-final { scan-assembler ".section\t.persistent.var_persistent,\"aw\"\n" } } */

/* Test the "persistent" attribute with -fdata-sections.  */
#include "attr-persistent-main.inc"
