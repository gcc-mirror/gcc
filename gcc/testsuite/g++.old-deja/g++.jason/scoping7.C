// { dg-do assemble  }
// Bug: g++ doesn't flag name collisions between types and non-types as
// errors.  It shouldn't for class names, but it should for typedefs.

int bar;			// { dg-error "" } 
typedef int bar;		// { dg-error "" } 
