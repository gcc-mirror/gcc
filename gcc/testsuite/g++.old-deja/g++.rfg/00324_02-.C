// { dg-do assemble  }
// Another simple one.  GCC corerctly gives errors for this code when the
// - -pedantic-errors options is used.  g++ doesn't.


void f (int i) { }
 
void (*fp)(void);
 
int i;
 
void
test ()
{
   i ? f : fp; // { dg-error "conditional expression|invalid conversion" } 
}
