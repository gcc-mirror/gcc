// Build don't link:

int i;
int &const j = i;		// ERROR - invalid const
int &const f();			// ERROR - invalid const
void g ()
{
  j = 1;
  f() = 1;
}
