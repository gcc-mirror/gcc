// Bug: g++ doesn't flag name collisions between types and non-types as
// errors.  It shouldn't for class names, but it should for typedefs.
// Build don't link:

int bar;			// ERROR - 
typedef int bar;		// ERROR - 
