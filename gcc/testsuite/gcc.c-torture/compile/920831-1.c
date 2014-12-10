/* { dg-require-effective-target indirect_jumps } */

f(x){goto*(char)x;}
