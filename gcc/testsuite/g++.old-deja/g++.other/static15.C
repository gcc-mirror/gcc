// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Option: -fdata-sections

void f()
{
  static int ctors[3] = { 0, 0, 0 };
  
  ctors[2] = 7;
}

int main ()
{
  f ();
}
