// { dg-do assemble  }
// Based on a testcase by Maciej Radziejewski <maciejr@iws.uni-stuttgart.de>

int i(0)(1); // { dg-error "" } multiple initialization
int j(2) = 3; // { dg-error "" } multiple initialization
int k(4)(5)(6); // { dg-error "" } multiple initialization
int m, n(7)(8); // { dg-error "" } multiple initialization
