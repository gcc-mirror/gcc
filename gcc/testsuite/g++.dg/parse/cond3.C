// PR c++/30847
// { dg-do compile }
// { dg-options "" }

int j, k, l;
extern void baz ();

void
foo (int i)
{
  (i ? j : k) = ({ l++; (void) l; });	// { dg-error "void value not ignored" }
  (i ? j : k) += ({ l++; (void) l; });	// { dg-error "void value not ignored" }
  (i ? j : k) = baz ();			// { dg-error "void value not ignored" }
  (i ? j : k) *= baz ();		// { dg-error "void value not ignored" }
}
