// Bug: g++ thought this was a redeclaration of a local variable.
// Build don't link:

int i;
int main ()
{
  extern int i;
}
