// Bug:  The conversion from bool to int gets stripped.
// Build don't link:

bool b;

int main ()
{
  return ((!b) != 0);
}
