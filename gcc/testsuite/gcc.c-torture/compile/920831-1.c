/* { dg-require-effective-target indirect_jumps } */
/* { dg-additional-options "-std=gnu89" } */

f(x){goto*(void *)x;}
