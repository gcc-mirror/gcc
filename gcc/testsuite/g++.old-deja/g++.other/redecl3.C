// { dg-do assemble  }
// Bug: g++ thought this was a redeclaration of a local variable.

int i;
int main ()
{
  extern int i;
}
