/* PR middle-end/10138 - warn for uninitialized arrays passed as const*
   arguments
   Verify that passing pointers to uninitialized objects to arguments
   to functions declared with attribute access is diagnosed where expected.
   { dg-do compile }
   { dg-options "-O -Wall" } */

#define RO(...) __attribute__ ((access (read_only, __VA_ARGS__)))
#define RW(...) __attribute__ ((access (read_write, __VA_ARGS__)))
#define WO(...) __attribute__ ((access (write_only, __VA_ARGS__)))

RO (1) void fpri (int*);      // { dg-message "in a call to 'fpri' declared with attribute 'access \\\(read_only, 1\\\)' here" }

RO (1) void fpcri (const int*);

RO (1, 2) void fpcri1_2 (const int*, int);


void warn_scalar_fpri (void)
{
  int i;                      // { dg-message "declared here" }
  fpri (&i);                  // { dg-warning "'i' is used uninitialized" }
}

void nowarn_scalar_plus_fpri (void)
{
  int i;
  /* This gets a -Wstringop-overflow for reading past the end but not
     -Wuninitialized because there's nothing to initialize there.  */
  fpri (&i + 1);              // { dg-warning "\\\[-Wstringop-overread" }
}

void nowarn_array_assign_fpcri (void)
{
  int a[2];
  a[0] = 0;
  fpcri (a);
}

void nowarn_array_init_fpcri (void)
{
  int a[4] = { 0 };
  fpcri (a);
}

void nowarn_array_compound_fpri (void)
{
  fpri ((int[2]){ 0 });
}

void nowarn_array_compound_fpcri (void)
{
  fpcri ((int[3]){ 1 });
}

void warn_scalar_fpcri (void)
{
  int i;
  fpcri (&i);                 // { dg-warning "\\\[-Wuninitialized" }
}

void warn_array_fpcri (void)
{
  int a[4];
  fpcri (a);                  // { dg-warning "\\\[-Wuninitialized" }
}

void warn_array_plus_cst_fpcri (void)
{
  int a[4];
  fpcri (a + 1);              // { dg-warning "\\\[-Wuninitialized" }
}

void warn_array_plus_var_fpcri (int i)
{
  int a[4];
  fpcri (a + i);              // { dg-warning "\\\[-Wuninitialized" }
}

void nowarn_struct_assign_fpcri (void)
{
  struct { int a, b; } s;
  s.a = 0;
  fpcri (&s.a);
}

void warn_struct_assign_fpcri (void)
{
  struct { int a, b; } s;
  s.a = 0;
  fpcri (&s.b);               // { dg-warning "\\\[-Wuninitialized" }
}

void nowarn_struct_init_fpcri (void)
{
  struct { int a, b; } s = { 0 };
  fpcri (&s.a);
  fpcri (&s.b);
}

void nowarn_struct_compound_fpcri (void)
{
  struct S { int a, b; };
  fpcri (&(struct S){ }.a);
  fpcri (&(struct S){ }.b);
}


void nowarn_scalar_fpcri1_2 (void)
{
  int i;
  fpcri1_2 (&i, 0);
}

void nowarn_array_assign_fpcri1_2 (void)
{
  int a[2];
  a[0] = 0;
  fpcri1_2 (a, 1);
}

void nowarn_array_assign_fpcri1_2_plus_cst (void)
{
  int a[3];
  a[1] = 0;
  fpcri1_2 (a + 1, 1);
}

void nowarn_array_init_fpcri1_2 (void)
{
  int a[4] = { 0 };
  fpcri1_2 (a, 2);
}

void warn_array_fpcri1_2_rd1 (void)
{
  int a[4];
  fpcri1_2 (a, 1);            // { dg-warning "\\\[-Wuninitialized" }
}

void warn_array_fpcri1_2_rd2 (void)
{
  int a[4];
  fpcri1_2 (a, 2);            // { dg-warning "\\\[-Wuninitialized" }
}
