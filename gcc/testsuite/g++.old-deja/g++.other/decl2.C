// Build don't link:
// Based on a test-case by Maciej Radziejewski <maciejr@iws.uni-stuttgart.de>

int i(0)(1); // ERROR - multiple initialization
int j(2) = 3; // ERROR - multiple initialization
int k(4)(5)(6); // ERROR - multiple initialization
int m, n(7)(8); // ERROR - multiple initialization
