/* PR middle-end/10138 - warn for uninitialized arrays passed as const*
   arguments
   Verify that passing pointers to uninitialized objects to arguments
   to functions declared with attribute access is diagnosed where expected.
   { dg-do compile }
   { dg-options "-O -Wall -ftrivial-auto-var-init=zero" } */

#define RW(...) __attribute__ ((access (read_write, __VA_ARGS__)))

RW (1) RW (3) void
f4pi (int*, int*, int*, int*);    // { dg-message "in a call to 'f4pi' declared with attribute 'access \\\(read_write, \[13\]\\\)'" }


void nowarn_scalar (void)
{
  int i1 = 0, i2, i3 = 1, i4;
  f4pi (&i1, &i2, &i3, &i4);
}

void warn_scalar_1 (void)
{
  int i1;                         // { dg-message "declared here" "" }
  int i2, i3 = 1, i4;

  f4pi (&i1, &i2, &i3, &i4);      // { dg-warning "'i1' may be used uninitialized" "" }
}

void warn_scalar_2 (void)
{
  int j1 = 0, j2, j4;
  int j3;

  f4pi (&j1, &j2, &j3, &j4);      // { dg-warning "'j3' may be used uninitialized" "" }
}


void nowarn_array_init (void)
{
  int a1[4] = { 0 }, a2[5], a3[6] = { 0 }, a4[7];

  f4pi (a1, a2, a3, a4);
}

void warn_array_1 (void)
{
  int a1[4];                  // { dg-message "'a1' declared here" }
  int a2[5], a3[6] = { 0 }, a4[7];

  f4pi (a1, a2, a3, a4);      // { dg-warning "'a1' may be used uninitialized" }
}

void warn_array_2 (void)
{
  int a1[4] = { 0 }, a2[5], a4[7];
  int a3[6];                  // { dg-message "'a3' declared here" }

  f4pi (a1, a2, a3, a4);      // { dg-warning "'a3' may be used uninitialized" }
}
