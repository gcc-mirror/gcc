// { dg-do compile }
// { dg-options "-Wbuiltin-declaration-mismatch" }

extern(C):

// Mismatched parameter lengths
double tan();             // { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" }

// Mismatched variadic arguments
int printf(const(char)*); // { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" }

// Mismatched return type
void puts(char*);         // { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" }

// Mismatched return storage class
ref int isalnum(int);     // { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" }

// Mismatched parameter type
double sin(long);         // { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" }

// Mismatched parameter storage class
double frexp(double, lazy int*); // { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" }
double log(ref double);          // { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" }

// Verify that storage classes don't affect covariance matching
@trusted nothrow @nogc pure double fabs(double);

// Verify inout is allowed instead of const
inout(char)* strstr(return scope inout(char)*, scope const char*) pure;

// Verify that FILE* is allowed as it is implicitly convertable to void*
struct _IO_FILE{}
alias FILE = shared(_IO_FILE);
int fprintf(FILE*, scope const char*, scope const ...);

// Verify integral types with same size are treated as if equivalent
int putchar(dchar);
