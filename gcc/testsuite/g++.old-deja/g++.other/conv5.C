// Build don't link:

// Based on bug report by Thomas Kunert <kunert@physik.tu-dresden.de>

// Special g++ Options:

int foo();
const int (*bar)() = foo; // ERROR - adding const - XFAIL *-*-*
