/* { dg-require-effective-target indirect_jumps } */

extern void*t[];x(i){goto*t[i];}
