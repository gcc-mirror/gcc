// Build don't link:
// Origin: Sergei Organov <osv@javad.ru>

void foo(void)
{
  extern int i;    // ERROR - previous declaration
  extern double i; // ERROR - conflicting type
  extern int j;
  extern int j;
}
