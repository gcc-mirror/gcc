/* PR middle-end/10138 - warn for uninitialized arrays passed as const*
   arguments
   { dg-do compile }
   { dg-options "-O -Wall" }
   { dg-require-effective-target alloca } */

typedef __SIZE_TYPE__ size_t;

void* alloca (size_t);
void* malloc (size_t);
void* realloc (void*, size_t);

void fpi (int*);
void fpci (const int*);
void fpcv (const void*);


void nowarn_scalar_fpi (void)
{
  int x;
  fpi (&x);
}

void nowarn_scalar_plus_cst_fpi (void)
{
  int x;
  // This deserves a warning other than -Wuninitialized.
  fpi (&x + 1);
}

void nowarn_scalar_plus_var_fpi (int i)
{
  int x;
  // Same as above, this deserves a warning other than -Wuninitialized.
  fpi (&x + i);
}

void nowarn_array_assign_fpci (void)
{
  int a[2];
  a[0] = 0;
  fpci (a);
}

void nowarn_array_assign_plus_cst_fpci (void)
{
  int a[4];
  a[1] = 0;
  a[2] = 1;
  fpci (a + 1);
}

void nowarn_array_init_fpci (void)
{
  int a[4] = { 0 };
  fpci (a);
}

void nowarn_array_compound_fpi (void)
{
  fpi ((int[2]){ 1 });
}

void nowarn_array_compound_fpci (void)
{
  fpci ((int[3]){ 1 });
}

void warn_array_fpci (void)
{
  int a[4];                   // { dg-message "declared here" }"
  fpci (a);                   // { dg-warning "\\\[-Wmaybe-uninitialized" }
}

void warn_array_plus_cst_fpci (void)
{
  int a[4];
  fpci (a + 1);               // { dg-warning "\\\[-Wmaybe-uninitialized" }
}

void warn_array_plus_var_fpci (int i)
{
  int a[4];
  fpci (a + i);               // { dg-warning "\\\[-Wmaybe-uninitialized" }
}

void nowarn_array_end_fpci (void)
{
  int a[4];
  /* This should be diagnosed by a warning other than -Wuninitialized
     because the just-past-the-end pointer cannot be dereferenced and
     the function doesn't take any other pointer to tell where the start
     of the array is.  -Wuninitialized isn't appropriate because there
     is nothing to initialize at that offset.  */
  fpci (a + 4);
}

void warn_matrix_fpcv (void)
{
  int a[2][2];
  fpci (a[1]);                // { dg-warning "\\\[-Wmaybe-uninitialized" }
}

void warn_scalar_fpcv (void)
{
  int i;
  fpci (&i);                  // { dg-warning "\\\[-Wmaybe-uninitialized" }
}

void warn_scalar_plus_cst_fpcv (void)
{
  int x;
  /* Same as above, this deserves a warning other than -Wuninitialized
     for passing the function a past-the-end pointer with no other
     argument.  */
  fpci (&x + 1);
}

void warn_scalar_plus_var_fpcv (int i)
{
  int x;
  fpci (&x + i);              // { dg-warning "\\\[-Wmaybe-uninitialized" }
}

void nowarn_struct_assign_fpci (void)
{
  struct { int a, b; } s;
  s.a = 0;
  fpci (&s.a);
}

void warn_struct_assign_fpci (void)
{
  struct { int a, b; } s;
  s.a = 0;
  fpci (&s.b);                // { dg-warning "\\\[-Wmaybe-uninitialized" }
}

void nowarn_struct_init_fpci (void)
{
  struct { int a, b; } s = { 0 };
  fpci (&s.a);
  fpci (&s.b);
}

void nowarn_struct_compound_fpci (void)
{
  struct S { int a, b; };
  fpci (&(struct S){ }.a);
  fpci (&(struct S){ }.b);
}

/* Verify that passing a just-past-the-end pointer to a const pointer
   argument to a function that takes another argument is not diagnosed
   since the two arguments together could outline a range.  */
void nowarn_fp_p (void)
{
  extern void fpi_pci (int*, const int*);

  {
    int i;
    fpi_pci (&i, &i + 1);
  }
  {
    int j;
    fpi_pci (&j + 1, &j + 1);
  }

  extern void fpc_pcc (char*, const char*);

  {
    char a[2];
    fpc_pcc (a, a + 2);
  }
  {
    char a[3];
    fpc_pcc (a, a + 3);
  }

  extern void fpcc_pcc (const char*, const char*);

  {
    char a[4];
    fpcc_pcc (a + 4, a + 4);
  }
}


/* Verify passing addresses of empty uninitialized objects doesn't
   trigger a warning.  */
void nowarn_fpcEmpty (void)
{
  struct Empty { };
  extern void fpcEmpty (const struct Empty*);

  /* Since Empty has no members warning for it isn't really necessary.
     See also PR 38908.  */
  struct Empty s;
  fpcEmpty (&s);
}


/* Verify passing addresses of uninitialized objects to functions
   declared without a proptotype doesn't trigger a warning.  */
void nowarn_noproto (void)
{
  extern void fnoproto ();
  int i, a[2];

  fnoproto (&i, a, a + 2);
}


/* Verify passing addresses of uninitialized objects to variadic
   functions doesn't trigger a warning.  */
void nowarn_vararg (void)
{
  extern void fvararg (int, ...);

  int i, a[2];

  fvararg (0, &i, a, a + 2);
}


void nowarn_alloca_assign_fpci (unsigned n)
{
  int *p = (int*)alloca (n);
  p[0] = 0;
  fpci (p);
}

void nowarn_alloca_assign_plus_cst_fpci (unsigned n)
{
  int *p = (int*)alloca (n);
  p[1] = 0;
  p[2] = 1;
  fpci (p + 1);
}

void warn_alloca_fpci (unsigned n)
{
  int *p = (int*)alloca (n);
  fpci (p);                   // { dg-warning "\\\[-Wmaybe-uninitialized" }
}

void warn_alloca_assign_plus_cst_fpci (unsigned n)
{
  int *p = (int*)alloca (n);
  p[1] = 0;
  p[2] = 1;
  fpci (p + 3);               // { dg-warning "\\\[-Wmaybe-uninitialized" }
}


void nowarn_vla_assign_fpci (unsigned n)
{
  int a[n];
  a[0] = 0;
  fpci (a);
}

void nowarn_vla_assign_plus_cst_fpci (unsigned n)
{
  int vla[n];
  vla[1] = 0;
  vla[2] = 1;
  fpci (vla + 1);
}

void warn_vla_fpci (unsigned n)
{
  int vla[n];                 // { dg-message "declared here" "pr?????" { xfail *-*-* } }"
  fpci (vla);                 // { dg-warning "\\\[-Wmaybe-uninitialized" }
}

void warn_vla_assign_plus_cst_fpci (unsigned n)
{
  int vla[n];                 // { dg-message "declared here" "pr?????" { xfail *-*-* } }"
  vla[1] = 0;
  vla[2] = 1;
  fpci (vla + 3);             // { dg-warning "\\\[-Wmaybe-uninitialized" }
}


void nowarn_malloc_assign_fpci (unsigned n)
{
  int *p = (int*)malloc (n);
  p[0] = 0;
  fpci (p);
}

void nowarn_malloc_assign_plus_cst_fpci (unsigned n)
{
  int *p = (int*)malloc (n);
  p[1] = 0;
  p[2] = 1;
  fpci (p + 1);
}

void warn_malloc_fpci (unsigned n)
{
  int *p = (int*)malloc (n);
  fpci (p);                   // { dg-warning "\\\[-Wmaybe-uninitialized" }
}

void warn_malloc_assign_plus_cst_fpci (unsigned n)
{
  int *p = (int*)malloc (n);  // { dg-message "allocated here" "pr?????" { xfail *-*-* } }"
  p[1] = 0;
  p[2] = 1;
  fpci (p + 3);               // { dg-warning "\\\[-Wmaybe-uninitialized" }
}
