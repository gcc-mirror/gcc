/* { dg-do run } */
/* { dg-skip-if "No dollar in identfiers" { avr-*-* powerpc-ibm-aix* } } */
/* { dg-skip-if "" { ! ucn } } */
/* { dg-options "-std=c99 -fdollars-in-identifiers -g" } */
void abort (void);

int a$b(void) { return 1; }
int a$b😀(void) { return 2; }

int main (void)
{
  
  if (a$b() != 1)
    abort ();

  if (a$b😀() != 2)
	  abort ();
  
  return 0;
}
