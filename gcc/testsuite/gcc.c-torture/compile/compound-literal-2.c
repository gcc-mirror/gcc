/* PR C/30265, invalid gimple was produced because we did not mark
   the compound literal's decl early enough.  */

int f(float *);
int g(float x)
{
  return f(&(float){x}) + f(&x);
}
