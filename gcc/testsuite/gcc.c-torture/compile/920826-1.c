/* { dg-require-effective-target indirect_jumps } */

void f(int*x){goto*(void*)(__INTPTR_TYPE__)(char)*x;}
