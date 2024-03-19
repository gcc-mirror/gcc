/* PR middle-end/97188 - ICE passing a null VLA to a function expecting
   at least one element
   { dg-do compile }
   { dg-options "-O -Wall -ftrack-macro-expansion=0" } */

#define INT_MAX   __INT_MAX__
#define INT_MIN   (-INT_MAX - 1)

/* Exercise passing nul to a one-dimensional VLA argument.  */

void test_fca_n (int r_m1)
{
  extern void fca_n (int n, char[n]);  // { dg-message "in a call to function 'fca_n'" "note" }

#define T(n) fca_n (n, 0)

  int min = INT_MIN;
  int max = INT_MAX;
  if (r_m1 >= 0)
    r_m1 = -1;

  // Verify negative bounds.
  T (min);          // { dg-warning "bound argument 1 value -\\d+ is negative for a variable length array argument 2 of type 'char\\\[n]'" }
  T (r_m1);         // { dg-warning "bound argument 1 value \\\[-\\d+, -1] is negative for a variable length array argument 2 of type 'char\\\[n]" }
  T ( -1);          // { dg-warning "bound argument 1 value -1 is negative for a variable length array argument 2 of type 'char\\\[n]" }

  T (  0);

  // Verify positive bounds.
  T (  1);          // { dg-bogus "argument 2 of variable length array 'char\\\[n]' is null but the corresponding bound argument 1 value is 1" }
  T (  9);          // { dg-bogus "argument 2 of variable length array 'char\\\[n]' is null but the corresponding bound argument 1 value is 9" }
  T (max);          // { dg-bogus "argument 2 of variable length array 'char\\\[n]' is null but the corresponding bound argument 1 value is \\d+" }
}


/* Exercise passing nul to an array with unspecified bound of VLAs.  */

void test_fsa_x_n (int r_m1)
{
  extern void fsa_x_n (int n, short[][n]);   // { dg-message "in a call to function 'fsa_x_n'" "note" }

#undef T
#define T(n) fsa_x_n (n, 0)

  int min = INT_MIN;
  int max = INT_MAX;
  if (r_m1 >= 0)
    r_m1 = -1;

  // Verify negative bounds.
  T (min);          // { dg-warning "bound argument 1 value -\\d+ is negative for a variable length array argument 2 of type 'short int\\\[]\\\[n]'" }
  T (r_m1);         // { dg-warning "bound argument 1 value \\\[-\\d+, -1] is negative for a variable length array argument 2 of type 'short int\\\[]\\\[n]" }
  T ( -1);          // { dg-warning "bound argument 1 value -1 is negative for a variable length array argument 2 of type 'short int\\\[]\\\[n]" }

  T (  0);

  // Verify positive bounds.
  T (  1);          // { dg-bogus "argument 2 of variable length array 'short int\\\[]\\\[n]' is null but the corresponding bound argument 1 value is 1" }
  T (  9);          // { dg-bogus "argument 2 of variable length array 'short int\\\[]\\\[n]' is null but the corresponding bound argument 1 value is 9" }
  T (max);          // { dg-bogus "argument 2 of variable length array 'short int\\\[]\\\[n]' is null but the corresponding bound argument 1 value is \\d+" }
}


/* Exercise passing nul to an array of a single VLA.  */

void test_fia_1_n (int r_m1)
{
  extern void fia_1_n (int n, int[1][n]);  // { dg-message "in a call to function 'fia_1_n'" "note" }

#undef T
#define T(n) fia_1_n (n, 0)

  int min = INT_MIN;
  int max = INT_MAX;
  if (r_m1 >= 0)
    r_m1 = -1;

  // Verify negative bounds.
  T (min);          // { dg-warning "bound argument 1 value -\\d+ is negative for a variable length array argument 2 of type 'int\\\[1]\\\[n]'" }
  T (r_m1);         // { dg-warning "bound argument 1 value \\\[-\\d+, -1] is negative for a variable length array argument 2 of type 'int\\\[1]\\\[n]" }
  T ( -1);          // { dg-warning "bound argument 1 value -1 is negative for a variable length array argument 2 of type 'int\\\[1]\\\[n]" }

  T (  0);

  // Verify positive bounds.
  T (  1);          // { dg-bogus "argument 2 of variable length array 'int\\\[1]\\\[n]' is null but the corresponding bound argument 1 value is 1" }
  T (  9);          // { dg-bogus "argument 2 of variable length array 'int\\\[1]\\\[n]' is null but the corresponding bound argument 1 value is 9" }
  T (max);          // { dg-bogus "argument 2 of variable length array 'int\\\[1]\\\[n]' is null but the corresponding bound argument 1 value is \\d+" }
}


/* Exercise passing nul to an array of three VLAs.  */

void test_fla_3_n (int r_m1)
{
  extern void fla_3_n (int n, long[3][n]);  // { dg-message "in a call to function 'fla_3_n'" "note" }

#undef T
#define T(n) fla_3_n (n, 0)

  int min = INT_MIN;
  int max = INT_MAX;
  if (r_m1 >= 0)
    r_m1 = -1;

  // Verify negative bounds.
  T (min);          // { dg-warning "bound argument 1 value -\\d+ is negative for a variable length array argument 2 of type 'long int\\\[3]\\\[n]'" }
  T (r_m1);         // { dg-warning "bound argument 1 value \\\[-\\d+, -1] is negative for a variable length array argument 2 of type 'long int\\\[3]\\\[n]" }
  T ( -1);          // { dg-warning "bound argument 1 value -1 is negative for a variable length array argument 2 of type 'long int\\\[3]\\\[n]" }

  T (  0);

  // Verify positive bounds.
  T (  1);          // { dg-bogus "argument 2 of variable length array 'long int\\\[3]\\\[n]' is null but the corresponding bound argument 1 value is 1" }
  T (  9);          // { dg-bogus "argument 2 of variable length array 'long int\\\[3]\\\[n]' is null but the corresponding bound argument 1 value is 9" }
  T (max);          // { dg-bogus "argument 2 of variable length array 'long int\\\[3]\\\[n]' is null but the corresponding bound argument 1 value is \\d+" }
}


/* Exercise passing nul to a VLA of five-element arrays.  */

void test_fda_n_5 (int r_m1)
{
  extern void fda_n_5 (int n, double[n][5]);// { dg-message "in a call to function 'fda_n_5'" "note" }

#undef T
#define T(n) fda_n_5 (n, 0)

  int min = INT_MIN;
  int max = INT_MAX;
  if (r_m1 >= 0)
    r_m1 = -1;

  // Verify negative bounds.
  T (min);          // { dg-warning "bound argument 1 value -\\d+ is negative for a variable length array argument 2 of type 'double\\\[n]\\\[5]'" }
  T (r_m1);         // { dg-warning "bound argument 1 value \\\[-\\d+, -1] is negative for a variable length array argument 2 of type 'double\\\[n]\\\[5]" }
  T ( -1);          // { dg-warning "bound argument 1 value -1 is negative for a variable length array argument 2 of type 'double\\\[n]\\\[5]" }

  T (  0);

  // Verify positive bounds.
  T (  1);          // { dg-bogus "argument 2 of variable length array 'double\\\[n]\\\[5]' is null but the corresponding bound argument 1 value is 1" }
  T (  9);          // { dg-bogus "argument 2 of variable length array 'double\\\[n]\\\[5]' is null but the corresponding bound argument 1 value is 9" }
  T (max);          // { dg-bogus "argument 2 of variable length array 'double\\\[n]\\\[5]' is null but the corresponding bound argument 1 value is \\d+" }
// { dg-warning "size 4294967256 exceeds maximum object size" "" { target ilp32 } .-1 }
}


/* Exercise passing nul to a two-dimensional VLA.  */

void test_fca_n_n (int r_m1)
{
  extern void fca_n_n (int n, char[n][n]);  // { dg-message "in a call to function 'fca_n_n'" "note" }

#undef T
#define T(n) fca_n_n (n, 0)

  int min = INT_MIN;
  int max = INT_MAX;
  if (r_m1 >= 0)
    r_m1 = -1;

  // Verify negative bounds.
  T (min);          // { dg-warning "bound argument 1 value -\\d+ is negative for a variable length array argument 2 of type 'char\\\[n]\\\[n]'" }
  T (r_m1);         // { dg-warning "bound argument 1 value \\\[-\\d+, -1] is negative for a variable length array argument 2 of type 'char\\\[n]\\\[n]" }
  T ( -1);          // { dg-warning "bound argument 1 value -1 is negative for a variable length array argument 2 of type 'char\\\[n]\\\[n]" }

  T (  0);

  // Verify positive bounds.
  T (  1);          // { dg-bogus "argument 2 of variable length array 'char\\\[n]\\\[n]' is null but the corresponding bound argument 1 value is 1" }
  T (  9);          // { dg-bogus "argument 2 of variable length array 'char\\\[n]\\\[n]' is null but the corresponding bound argument 1 value is 9" }
  T (max);          // { dg-bogus "argument 2 of variable length array 'char\\\[n]\\\[n]' is null but the corresponding bound argument 1 value is \\d+" }
}
