/* The following code used to ICE in fold_convert.  */

float ceilf(float);

int foo(float x)
{
  return (double)ceilf(x);
}

