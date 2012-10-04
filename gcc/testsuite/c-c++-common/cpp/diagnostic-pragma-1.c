// { dg-do compile }

#pragma GCC warning "warn-a" // { dg-warning warn-a }
#pragma GCC error "err-b" // { dg-error err-b }

#define CONST1 _Pragma("GCC warning \"warn-c\"") 1
#define CONST2 _Pragma("GCC error \"err-d\"") 2

char a[CONST1]; // { dg-warning warn-c }
char b[CONST2]; // { dg-error err-d }

