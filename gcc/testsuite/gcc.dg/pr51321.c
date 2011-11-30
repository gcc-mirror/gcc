/* { dg-do compile } */
int main ()
{
  return (__builtin_types_compatible_p (char[1][], char[1][1])); /* { dg-error "array type has incomplete element type" } */
}


