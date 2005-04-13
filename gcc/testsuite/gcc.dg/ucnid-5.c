/* { dg-do run } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "*" } { "" } } */
/* { dg-options "-std=c99 -fdollars-in-identifiers" } */
void abort (void);

int a$b(void) { return 1; }

int main (void)
{
  
  if (a\u0024b() != 1)
    abort ();
  
  return 0;
}
