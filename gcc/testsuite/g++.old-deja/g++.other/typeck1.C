// Build don't link:

extern int a[][];   // ERROR - invalid multidimensional array
extern int b[7][];  // ERROR - invalid multidimensional array
extern int c[][7];  // OK

extern int (*i)[];  // ERROR - previous declaration
extern int (*i)[7]; // ERROR - conflicting types for `i'

extern int m[];
extern int m[7];    // OK

void f(int (*j)[3])
{
  extern int (*k)[];
  f(k);             // ERROR - passing wrong type
}
