// Build don't link:

void foo ()
{
  int e;
  e := e;	// ERROR - parse error
}
