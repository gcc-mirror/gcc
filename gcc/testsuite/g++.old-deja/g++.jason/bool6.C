// { dg-do assemble  }
// Bug:  The conversion from bool to int gets stripped.

bool b;

int main ()
{
  return ((!b) != 0);
}
