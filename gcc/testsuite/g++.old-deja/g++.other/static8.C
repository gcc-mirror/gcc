// Build don't link:
// Special g++ Options: -fno-squangle
// Origin: Mark Mitchell <mark@codesourcery.com>

static unsigned int strlen (const char*) {} // ERROR - previous declaration

int strlen__FPCc = 0; // ERROR - duplicate declaration
