// { dg-do assemble  }
// { dg-options "" }

// Based on bug report by Thomas Kunert <kunert@physik.tu-dresden.de>


int foo();
const int (*bar)() = foo; // { dg-error "" } adding const
