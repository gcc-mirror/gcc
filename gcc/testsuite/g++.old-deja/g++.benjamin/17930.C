// 981204 bkoz
// g++/17930
// Build don't link:

char const one[] = "test";
char const two[] = one; // ERROR - // ERROR -
