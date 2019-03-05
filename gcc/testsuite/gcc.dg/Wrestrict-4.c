/* PR tree-optimization/83603 - ICE in builtin_memref at
   gcc/gimple-ssa-warn-restrict.c:238
   Test to verify that invalid calls to built-in functions declared
   without a prototype don't cause an ICE.
   { dg-do compile }
   { dg-prune-output "conflicting types for built-in" }
   { dg-options "-O2 -Warray-bounds -Wrestrict" } */

void* memcpy ();
void* memmove ();
char* stpcpy ();
char* strcat ();
char* strcpy ();
char* strncat ();
char* strncpy ();

void* test_memcpy_0 ()
{
  return memcpy ();
}

void* test_memcpy_1 (void *d)
{
  return memcpy (d);
}

void* test_memcpy_2 (void *d, const void *s)
{
  return memcpy (d, s);
}


void* test_memmove_0 ()
{
  return memmove ();
}

void* test_memmove_1 (void *d)
{
  return memmove (d);
}

void* test_memmove_2 (void *d, const void *s)
{
  return memmove (d, s);
}


void* test_stpcpy_0 ()
{
  return stpcpy ();
}

void* test_stpcpy_1 (char *d)
{
  return stpcpy (d);
}


char* test_strcat_0 ()
{
  return strcat ();
}

char* test_strcat_1 (char *d)
{
  return strcat (d);
}


void* test_strcpy_0 ()
{
  return strcpy ();
}

void* test_strcpy_1 (char *d)
{
  return strcpy (d);
}


char* test_strncat_0 ()
{
  return strncat ();
}

char* test_strncat_1 (char *d)
{
  return strncat (d);
}

char* test_strncat_2 (char *d, const char *s)
{
  return strncat (d, s);
}


void* test_strncpy_0 ()
{
  return strncpy ();
}

void* test_strncpy_1 (char *d)
{
  return strncpy (d);
}

void* test_strncpy_2 (char *d, const char *s)
{
  return strncpy (d, s);
}

/* { dg-prune-output "\\\[-Wbuiltin-declaration-mismatch]" } */
