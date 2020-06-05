/* PR middle-end/10138 - warn for uninitialized arrays passed as const*
   arguments
   Verify that passing pointers to uninitialized objects to const
   arguments to built-ins is diagnosed where expected.
   { dg-do compile }
   { dg-options "-O -Wall" } */

typedef __SIZE_TYPE__ size_t;

void* alloca (size_t);
void* malloc (size_t);
void* realloc (void*, size_t);

void* memcpy (void*, const void*, size_t);
char* strcpy (char*, const char*);
size_t strlen (const char*);

void sink (void*);

void nowarn_array_memcpy (void *d, unsigned n)
{
  int a[2];
  /* Diagnose this?  */
  memcpy (d, a, n /* Non-constant to avoid folding into MEM_REF.  */);
}

void nowarn_array_plus_cst_memcpy (void *d, unsigned n)
{
  int a[3];
  /* Diagnose this?  */
  memcpy (d, a + 1, n);
}

void nowarn_array_plus_var_memcpy (void *d, unsigned n, int i)
{
  int a[4];
  /* Diagnose this?  */
  memcpy (d, a + i, n);
}

void nowarn_array_assign_memcpy (char *d, unsigned n)
{
  int a[3];
  a[1] = 3;
  memcpy (d, a, n);
}

void nowarn_array_init_memcpy (char *d, unsigned n)
{
  int a[4] = { 0 };
  memcpy (d, a, n);
}

void nowarn_array_compound_memcpy (void *d, unsigned n)
{
  memcpy (d, (int[2]){ 0 }, n);
}

void nowarn_struct_assign_memcpy (void *d, unsigned n)
{
  struct S { int a, b, c, d; } s;
  s.b = 1;
  s.d = 2;
  memcpy (d, &s, n);
}


void nowarn_array_init_strcpy (char *d[], unsigned n)
{
  char a[8] = "012";

  strcpy (d[0], a);
  strcpy (d[1], a + 1);
  strcpy (d[1], a + 2);
  strcpy (d[1], a + 3);
  strcpy (d[1], a + 4);
  strcpy (d[1], a + 5);
  strcpy (d[1], a + 6);
  strcpy (d[1], a + 7);
}


void nowarn_array_assign_strcpy (char *d[], unsigned n)
{
  char a[8];
  a[0] = '0';
  a[1] = '1';
  a[2] = '2';
  a[3] = '\0';

  strcpy (d[0], a);
  strcpy (d[1], a + 1);
  strcpy (d[1], a + 2);
  strcpy (d[1], a + 3);
}

void warn_array_plus_cst_strcpy (char *d, unsigned n)
{
  char a[8];
  a[0] = '1';
  a[1] = '2';
  a[2] = '3';
  a[3] = '\0';

  strcpy (d, a + 4);          // { dg-warning "\\\[-Wuninitialized" }
  strcpy (d, a + 5);          // { dg-warning "\\\[-Wuninitialized" }
  strcpy (d, a + 6);          // { dg-warning "\\\[-Wuninitialized" }
  strcpy (d, a + 7);          // { dg-warning "\\\[-Wuninitialized" }
}

void nowarn_array_plus_var_strcpy (char *d, int i)
{
  char a[8];
  a[0] = '1';
  a[1] = '2';
  a[2] = '3';
  a[3] = '\0';

  strcpy (d, a + i);
}


size_t nowarn_array_assign_strlen (const char *s)
{
  char a[8];
  a[0] = s[0];
  a[1] = s[1];
  a[2] = s[2];
  a[3] = s[3];

  size_t n = 0;

  n += strlen (a);
  n += strlen (a + 1);
  n += strlen (a + 2);
  n += strlen (a + 3);
  return n;
}

size_t warn_array_plus_cst_strlen (const char *s)
{
  char a[8];
  a[0] = s[0];
  a[1] = s[1];
  a[2] = s[2];
  a[3] = s[3];

  return strlen (a + 4);      // { dg-warning "\\\[-Wuninitialized" }
}

size_t nowarn_array_plus_var_strlen (const char *s, int i)
{
  char a[8];
  a[0] = s[0];
  a[1] = s[1];
  a[2] = s[2];
  a[3] = s[3];

  return strlen (a + i);
}


size_t nowarn_alloca_assign_strlen (int i)
{
  char *p = (char*)alloca (8);
  p[i] = '\0';
  return strlen (p);
}

size_t nowarn_alloca_escape_strlen (int i)
{
  char *p = (char*)alloca (8);
  sink (p);
  return strlen (p);
}

size_t warn_alloca_strlen (void)
{
  char *p = (char*)alloca (8);
  return strlen (p);          // { dg-warning "\\\[-Wuninitialized" }
}


size_t nowarn_malloc_assign_strlen (int i)
{
  char *p = (char*)malloc (8);
  p[i] = '\0';
  return strlen (p);
}

size_t nowarn_malloc_escape_strlen (int i)
{
  char *p = (char*)malloc (8);
  sink (p);
  return strlen (p);
}

size_t warn_malloc_strlen (void)
{
  char *p = (char*)malloc (8);
  return strlen (p);          // { dg-warning "\\\[-Wuninitialized" }
}


size_t nowarn_realloc_strlen (void *p)
{
  char *q = (char*)realloc (p, 8);
  return strlen (q);
}


size_t nowarn_vla_assign_strlen (int n, int i)
{
  char vla[n];
  vla[i] = '\0';
  return strlen (vla);
}

size_t nowarn_vla_strcpy_strlen (int n, const char *s, int i)
{
  char vla[n];
  strcpy (vla, s);
  return strlen (vla + i);
}

size_t nowarn_vla_escape_strlen (int n, int i)
{
  char vla[n];
  sink (vla);
  return strlen (vla);
}

size_t warn_vla_strlen (unsigned n)
{
  char vla[n];
  return strlen (vla);        // { dg-warning "\\\[-Wuninitialized" }
}
