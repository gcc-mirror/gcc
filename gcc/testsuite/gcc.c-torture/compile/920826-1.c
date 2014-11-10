/* { dg-require-effective-target indirect_jumps } */

f(int*x){goto*(char)*x;}
