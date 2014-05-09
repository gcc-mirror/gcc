// DR 587
// PR c++/51317

int x = 1;
int const y = 2;
int const *p = &(1 ? x : y); // error: lvalue required as unary '&' operand
