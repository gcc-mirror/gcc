// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

static unsigned int strlen (const char*) {} // ERROR - previous declaration

int _Z6strlenPKc = 0; // ERROR - duplicate declaration
