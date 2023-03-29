/* { dg-do link } */
/* { dg-do run { target { vma_equals_lma } } } */
/* { dg-require-effective-target noinit } */
/* { dg-options "-flto -save-temps" } */
/* { dg-skip-if "data LMA != VMA" { msp430-*-* } { "-mlarge" } } */
/* { dg-final { scan-file attr-noinit-3.ltrans0.ltrans.s ".section\t\.noinit,\"aw\"\n" } } */

/* Test the "noinit" attribute with -flto.  Specifically examine the
   final LTO assembly file, to ensure the "noinit" setting on the variable
   hasn't been lost.  */
#include "attr-noinit-main.inc"

