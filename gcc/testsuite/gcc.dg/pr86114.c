/* PR tree-optimization/86114 - ICE in gimple_fold_builtin_strlen with
   an invalid call to strnlen
   { dg-do compile }
   { dg-options "-O2" }  */

typedef __SIZE_TYPE__ size_t;

extern char* strcmp (const char*, const char*);
extern char* strncmp (const char*, const char*, size_t);
extern char* strlen (const char*);
extern char* strnlen (const char*, size_t);
extern char* strcspn (const char*, const char*);
extern char* strspn (const char*, const char*);
extern char* strxfrm (const char*, const char*, size_t);

char** q;

void test_array (const char *s)
{
  extern char a[8];

  q[0] = strcmp (a, s);
  q[1] = strncmp (a, s, 7);
  q[2] = strlen (a);
  q[3] = strnlen (a, 7);
  q[4] = strcspn (a, s);
  q[5] = strspn (a, s);
  q[6] = strxfrm (a, s, 7);
}

void test_pointer (const char *s, const char *t)
{
  q[0] = strcmp (s, t);
  q[1] = strncmp (s, t, 7);
  q[2] = strlen (s);
  q[3] = strnlen (s, 7);
  q[4] = strcspn (s, t);
  q[5] = strspn (s, t);
  q[6] = strxfrm (s, s, 7);
}

/* { dg-prune-output "-Wbuiltin-declaration-mismatch" } */
