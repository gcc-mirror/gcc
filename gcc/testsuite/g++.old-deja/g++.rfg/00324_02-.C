// Another simple one.  GCC corerctly gives errors for this code when the
// - -pedantic-errors options is used.  g++ doesn't.

// Build don't link:

void f (int i) { }
 
void (*fp)(void);
 
int i;
 
void
test ()
{
   i ? f : fp; // ERROR - 
}
