long double dfrom = 1.1;
long double m1;
long double m2;
unsigned long mant_long;

int main( )
{
  m1 = dfrom / 2.0;

  m2 = m1 * 4294967296.0;
  mant_long = ((unsigned long) m2) & 0xffffffff;

  if ( mant_long == 0x8ccccccc)
    exit (0);
  else
    abort();
}
