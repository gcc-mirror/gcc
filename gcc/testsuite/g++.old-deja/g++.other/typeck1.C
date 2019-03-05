// { dg-do assemble  }

extern int a[][];   // { dg-error "12:declaration of .a. as multidimensional array" } invalid multidimensional array
extern int b[7][];  // { dg-error "12:declaration of .b. as multidimensional array" } invalid multidimensional array
extern int c[][7];  // OK

extern int (*i)[];  // { dg-message "" } previous declaration
extern int (*i)[7]; // { dg-error "" } conflicting types for `i'

extern int m[];
extern int m[7];    // OK

void f(int (*j)[3])
{
  extern int (*k)[];
  f(k);             // { dg-error "" } passing wrong type
}
