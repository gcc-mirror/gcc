// { dg-do compile }
// { dg-options -O2 }

float f (float x)
{
  return __builtin_pow[1] (x, 2); // { dg-warning "pointer to a function used in arithmetic" }
}
