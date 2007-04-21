/* PR C/30265, invalid gimple was produced because we did not mark
   the compound literal's decl early enough.  */

int f(_Complex float *);
int g(_Complex float x)
{
  return f(&(_Complex float){x+1}) + f(&x);
}
