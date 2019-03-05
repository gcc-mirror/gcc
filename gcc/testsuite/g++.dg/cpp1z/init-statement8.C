// { dg-do compile { target c++17 } }

int
f ()
{
  if (int c = 5;
      int c = 5) // { dg-error "redeclaration" }
    return 5;
  return 0;
}
