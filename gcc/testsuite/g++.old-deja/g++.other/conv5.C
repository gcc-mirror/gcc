// Build don't link:

// Based on bug report by Thomas Kunert <kunert@physik.tu-dresden.de>

const int foo();
int (*bar)() = foo; // ERROR - discarding const - XFAIL *-*-*
