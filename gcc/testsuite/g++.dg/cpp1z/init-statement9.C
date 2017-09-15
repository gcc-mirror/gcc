// { dg-options -std=c++17 }

void
f ()
{
  {
    int c;
    if (int c = 2; c != 0)
      int c = 4; // { dg-error "redeclaration" }
  }

  if (int c = 2; c != 0)
    int c = 4; // { dg-error "redeclaration" }

  if (int c = 2; int c = 6) // { dg-error "redeclaration" }
    int c = 4; // { dg-error "redeclaration" }
}
