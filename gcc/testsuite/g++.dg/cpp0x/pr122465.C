// PR c++/122465
// { dg-do compile { target c++11 } }

void
foo ()
{    
  int x = 0;
  for (const T i = { i } : x)	// { dg-error "'T' does not name a type" }
    ;
}
