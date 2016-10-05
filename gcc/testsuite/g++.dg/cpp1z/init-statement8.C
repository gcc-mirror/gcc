// { dg-options -std=c++1z }

int
f ()
{
  if (int c = 5;
      int c = 5) // { dg-error "redeclaration" }
    return 5;
  return 0;
}
// { dg-options -std=c++1z }

int
f ()
{
  if (int c = 5;
      int c = 5) // { dg-error "redeclaration" }
    return 5;
  return 0;
}
