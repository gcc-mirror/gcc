// { dg-do assemble  }
// Origin: Sergei Organov <osv@javad.ru>

void foo(void)
{
  extern int i;    // { dg-error "" } previous declaration
  extern double i; // { dg-error "" } conflicting type
  extern int j;
  extern int j;
}
