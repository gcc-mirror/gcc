// { dg-do assemble  }
// { dg-options "" }
// Check association of {error} for Koenig lookup

struct QString { operator void * (); };
int foo()
{
  QString bar;
  return (bar == __null );
}
