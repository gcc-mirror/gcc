// PR 5310
// { dg-do compile }
// { dg-options "-pedantic -Wall -Wextra -Wpointer-arith -Wconversion" } 
void foo (int);
void foo (long);

void bar()
{
   foo ((int)__null);
   foo ((long)__null);
}
