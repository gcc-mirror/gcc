/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

extern char* stpcpy (char*, const char*);

int f (char* s)
{
  char a[32];

  __builtin___strcpy_chk (a, s, __builtin_object_size (a, 1));

  return __builtin_strlen (a);
}
