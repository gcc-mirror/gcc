// Build don't link:
// Check association of {error} for Koenig lookup
// Special g++ Options:

struct QString { operator void * (); };
int foo()
{
  QString bar;
  return (bar == __null );
}
