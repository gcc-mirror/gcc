/* { dg-do link } */
/* { dg-do run { target { vma_equals_lma } } } */
/* { dg-require-effective-target persistent } */
/* { dg-options "-flto -save-temps" } */
/* { dg-skip-if "data LMA != VMA" { msp430-*-* } { "-mlarge" } } */
/* { dg-final { scan-file attr-persistent-3.ltrans0.ltrans.s ".section\t\.persistent,\"aw\"\n" } } */

/* Test the "persistent" attribute with -flto.  Specifically examine the
   final LTO assembly file, to ensure the "persistent" setting on the variable
   hasn't been lost.  */
#include "attr-persistent-main.inc"
