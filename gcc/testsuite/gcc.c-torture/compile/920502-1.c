/* { dg-require-effective-target indirect_jumps } */
/* { dg-additional-options "-std=gnu89" } */

extern void*t[];x(i){goto*t[i];}
