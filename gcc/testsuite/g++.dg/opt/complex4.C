// PR 24362
// { dg-do compile }
// { dg-options "-O2" }

typedef __complex__ double cdouble;
cdouble elt_zero();
const cdouble *pointer();

cdouble trace(void)
{
  cdouble output = elt_zero();
  const cdouble *data = pointer();
  output += data[1];
  return output;
}

