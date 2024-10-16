/* PR c/83656 - missing -Wbuiltin-declaration-mismatch on declaration
   without prototype
   { dg-do compile }
   { dg-options "-std=gnu17 -fpermissive -Wbuiltin-declaration-mismatch" } */

typedef __SIZE_TYPE__ size_t;

/* Built-ins declared without a prototype are not diagnosed by default
   (without -Wextra) except when their return type doesn't match.  */
int abort ();       /* { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" } */

/* Built-ins declared without a prototype are not diagnosed without -Wextra.  */
void exit ();
void* memcpy ();
void* memset ();


void test_call_abort (void)
{
  /* Verify that a valid call to abort() is not diagnosed.  */
  abort ();

  /* Unfortunately, the incompatible declaration above makes GCC "forget"
     that abort() is a built-in and so the invalid calls below aren't
     diagnosed.  The only saving grace is that the invalid declaration
     that differs in the return type is diagnosed by default. */
  abort (1);        /* { dg-warning "too many arguments to built-in function .abort. expecting 0" "pr?????" { xfail *-*-* } } */

  abort (1, 2);     /* { dg-warning "too many arguments" "pr?????" { xfail *-*-* } } */
}


void test_call_exit (void)
{
  /* Verify that valid calls to exit are not diagnosed.  */
  exit ('\0');
  exit (0);

  /* Also verify calls to the built-in.  */
  __builtin_exit ('\0');
  __builtin_exit (0);
  __builtin_exit (0.0);

  exit ();          /* { dg-warning "too few arguments to built-in function 'exit' expecting 1" } */

  exit (1, 2);      /* { dg-warning "too many arguments" } */

  /* Verify that passing incompatible arguments triggers a warning.  */
  exit ("");        /* { dg-warning "\\\[-Wint-conversion]" } */

  struct S { int i; } s = { 0 };
  exit (s);         /* { dg-warning "incompatible type for argument 1" } */
}


void test_call_memcpy (void *p, const void *q, size_t n)
{
  memcpy (p, q, n);

  memcpy ();        /* { dg-warning "too few arguments to built-in function 'memcpy' expecting 3" } */

  memcpy (p);       /* { dg-warning "too few arguments to built-in function 'memcpy' expecting 3" } */

  memcpy (p, q);     /* { dg-warning "too few arguments to built-in function 'memcpy' expecting 3" } */

  memcpy (q, p, n); /* { dg-warning "\\\[-Wdiscarded-qualifiers]" } */

  memcpy (p, n, q); /* { dg-warning "\\\[-Wint-conversion]" } */

  memcpy (p, q, n, 0); /* { dg-warning "too many arguments to built-in function 'memcpy' expecting 3" } */
}


typedef void* (memcpy_t)(void*, const void*, size_t);
typedef void* (memset_t)(void*, int, size_t);

void test_init (void)
{
  /* Verify that initialization of a pointer by the address of a built-in
     function of a matching type declared without a prototype doesn't
     trigger a warning...  */
  memset_t *pmemset = memset;

  /* ...but initialization by the address of an incompatible built-in
     does even without -Wextra.  */
  memcpy_t *pmemcpy = memset;           /* { dg-warning "\\\[-Wincompatible-pointer-types]" } */
}


void test_assign (void)
{
  /* Same as above but for assignment.  */
  memset_t *pmemset;
  pmemset = memset;

  memcpy_t *pmemcpy;
  pmemcpy = memset;                     /* { dg-warning "\\\[-Wincompatible-pointer-types]" } */
}


/* Verify that passing built-ins declared without a prototype to
   functions that expect a pointer to a function of a specific type
   is diagnosed.  Ditto for return statements.  */

void take_memcpy (memcpy_t*);
void take_any (int, ...);

memset_t* pass_args (int i)
{
  take_memcpy (memcpy);
  take_memcpy (memset);                 /* { dg-warning "\\\[-Wincompatible-pointer-types]" } */

  take_any (0, i ? memcpy : memset);    /* { dg-warning "\\\[-Wincompatible-pointer-types]" } */

  return memcpy;                        /* { dg-warning "\\\[-Wincompatible-pointer-types]" } */
}
