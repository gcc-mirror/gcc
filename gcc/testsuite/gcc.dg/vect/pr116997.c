/* PR 116997.  */
struct S0
{
  unsigned f0;
  signed f2 : 11;
  signed : 6;
} GlobS, *Ptr = &GlobS;

const struct S0 Initializer = {7, 3};

int main (void)
{
  for (unsigned i = 0; i <= 2; i++)
    *Ptr = Initializer;
  if (GlobS.f2 != 3)
    __builtin_abort ();
  return 0;
}
