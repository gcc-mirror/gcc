/* { dg-require-effective-target indirect_jumps } */

f(int*x){goto*(void*)(__INTPTR_TYPE__)(char)*x;}
