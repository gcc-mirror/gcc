// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

int i;
int j;

void
f ()
{
  j = j + (i ? 7 : throw 1);
}
 
