// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

#if (!defined (__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100)
#define NAME(OLD, NEW) OLD
#else
#define NAME(OLD, NEW) NEW
#endif /* (!defined (__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100) */

static unsigned int strlen (const char*) {return 0;} // ERROR - previous declaration

int NAME (strlen__FPCc, _Z6strlenPKc) = 0; // ERROR - duplicate declaration
