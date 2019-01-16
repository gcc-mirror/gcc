/* PR c/83656 - missing -Wbuiltin-declaration-mismatch on declaration
   without prototype
   { dg-do compile }
   { dg-options "-Wbuiltin-declaration-mismatch" } */

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;

char c;
signed char sc;
unsigned char uc;
short si;
unsigned short usi;
int i;
unsigned ui;
long li;
unsigned long uli;

size_t szi;
typedef size_t SizeType;
SizeType szti;

ptrdiff_t diffi;

enum E { e0 } e;

float f;
double d;
long double ld;


/* Verify warnings for undefined calls to built-ins expecting integer
   arguments.  */

int abs ();         /* { dg-message "built-in .abs. declared here" } */

void test_integer_conversion_abs (void)
{
  i = abs (c);
  i = abs (sc);
  i = abs (uc);

  i = abs (si);
  i = abs (usi);

  i = abs (i);
  i = abs (ui);     /* { dg-warning ".abs. argument 1 type is .unsigned int. where .int. is expected in a call to built-in function declared without prototype" } */

  /* Verify that the same call as above but to the built-in doesn't
     trigger a warning.  */
  i = __builtin_abs (ui);

  i = abs (li);     /* { dg-warning ".abs. argument 1 type is .long int. where .int. is expected in a call to built-in function declared without prototype" } */
  i = abs (uli);    /* { dg-warning ".abs. argument 1 type is .long unsigned int. where .int. is expected in a call to built-in function declared without prototype" } */

  i = abs (e0);
  i = abs (e);

  i = abs (-1.0);   /* { dg-warning ".abs. argument 1 type is .double. where .int. is expected in a call to built-in function declared without prototype" } */
  i = abs (f);      /* { dg-warning ".abs. argument 1 promotes to .double. where .int. is expected in a call to built-in function declared without prototype" } */
  i = abs (ld);     /* { dg-warning ".abs. argument 1 type is .long double. where .int. is expected in a call to built-in function declared without prototype" } */

  /* Verify that the same call as above but to the built-in doesn't
     trigger a warning.  */
  i = __builtin_abs (ld);
}


extern void* memset ();

void test_integer_conversion_memset (void *d)
{
  memset (d, 0, sizeof (int));
  memset (d, '\0', szi);
  memset (d, i, szti);

  /* Passing a ptrdiff_t where size_t is expected may not be unsafe
     but because GCC may emits suboptimal code for such calls warning
     for them helps improve efficiency.  */
  memset (d, 0, diffi);       /* { dg-warning ".memset. argument 3 promotes to .ptrdiff_t. {aka .\(long \)?int.} where .\(long \)?unsigned int. is expected" } */

  memset (d, 0, 2.0);         /* { dg-warning ".memset. argument 3 type is .double. where '\(long \)?unsigned int' is expected" } */

  /* Verify that the same call as above but to the built-in doesn't
     trigger a warning.  */
  __builtin_memset (d, 0.0, 4.0);
}


/* Verify warnings for undefined calls to built-ins expecting floating
   arguments.  */

double fabs ();           /* { dg-message "built-in .fabs. declared here" } */

/* Expect a warning for fabsf below because even a float argument promotes
   to double.  Unfortunately, invalid calls to fabsf() are not diagnosed.  */
float fabsf ();           /* { dg-warning "conflicting types for built-in function .fabsf.; expected .float\\\(float\\\)." } */
long double fabsl ();     /* { dg-message "built-in .fabsl. declared here" } */

void test_real_conversion_fabs (void)
{
  d = fabs (c);     /* { dg-warning ".fabs. argument 1 promotes to .int. where .double. is expected in a call to built-in function declared without prototype" } */

  d = fabs (i);     /* { dg-warning ".fabs. argument 1 type is .int. where .double. is expected in a call to built-in function declared without prototype" } */

  d = fabs (li);    /* { dg-warning ".fabs. argument 1 type is .long int. where .double. is expected in a call to built-in function declared without prototype" } */

  /* In C, the type of an enumeration constant is int.  */
  d = fabs (e0);    /* { dg-warning ".fabs. argument 1 type is .int. where .double. is expected in a call to built-in function declared without prototype" } */

  d = fabs (e);     /* { dg-warning ".fabs. argument 1 type is .enum E. where .double. is expected in a call to built-in function declared without prototype" "ordinary enum" { target { ! short_enums } } } */
  /* { dg-warning ".fabs. argument 1 promotes to .int. where .double. is expected in a call to built-in function declared without prototype" "size 1 enum" { target short_enums } .-1 } */

  /* No warning here since float is promoted to double.  */
  d = fabs (f);

  d = fabs (ld);    /* { dg-warning ".fabs. argument 1 type is .long double. where .double. is expected in a call to built-in function declared without prototype" } */

  d = fabsf (c);    /* { dg-warning ".fabsf. argument 1 promotes to .int. where .float. is expected in a call to built-in function declared without prototype" "pr87890" { xfail *-*-* } } */

  d = fabsl (c);    /* { dg-warning ".fabsl. argument 1 promotes to .int. where .long double. is expected in a call to built-in function declared without prototype" } */

  d = fabsl (f);    /* { dg-warning ".fabsl. argument 1 promotes to .double. where .long double. is expected in a call to built-in function declared without prototype" } */

  /* Verify that the same call as above but to the built-in doesn't
     trigger a warning.  */
  d = __builtin_fabsl (f);
}

/* Verify warnings for calls to a two-argument real function.  */

double pow ();      /* { dg-message "built-in .pow. declared here" } */

void test_real_conversion_pow (void)
{
  d = pow (2.0, 2.0);
  d = pow (d, 3.0);
  d = pow (d, d);

  d = pow (2, 3.0); /* { dg-warning ".pow. argument 1 type is .int. where .double. is expected in a call to built-in function declared without prototype" } */
  d = pow (3.0, 2); /* { dg-warning ".pow. argument 2 type is .int. where .double. is expected in a call to built-in function declared without prototype" } */
}


/* Verify warnings for calls that discard qualifiers.  */

extern void* memcpy ();

void test_qual_conversion_memcpy (void *d, const void *s)
{
  memcpy (d, s, sizeof (int));
  memcpy (s, d, sizeof (int));    /* { dg-warning "passing argument 1 of .memcpy. discards 'const' qualifier from pointer target type" } */
}
