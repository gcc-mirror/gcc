// Build don't link:

enum tristate { no = -1, maybe, yes };

void foobar ()
{
  tristate var = no;		// gets bogus error
}
