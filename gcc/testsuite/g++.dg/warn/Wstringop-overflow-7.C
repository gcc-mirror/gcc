/* PR c++/100876 - -Wmismatched-new-delete should either look through
   or ignore placement new
   { dg-do compile }
   { dg-options "-O0 -Wall -Wno-array-bounds" } */

inline void* operator new (__SIZE_TYPE__, void *p) { return p; }
inline void* operator new[] (__SIZE_TYPE__, void *p) { return p; }

void* nowarn_placement_new_memset ()
{
  struct S { int i; };
  void *p = __builtin_malloc (sizeof (S));
  S *q = new (p) S;
  __builtin_memset (q, 0, sizeof (S));
  return q;
}

void* warn_placement_new_memset ()
{
  struct S { int i; };
  void *p = __builtin_malloc (sizeof (S));
  S *q = new (p) S;
  __builtin_memset (q, 0, sizeof (S) + 1);  // { dg-warning "\\\[-Wstringop-overflow" }
  return q;
}

void* nowarn_placement_new_array_strncpy (const char *s)
{
  void *p = __builtin_malloc (5);
  char *q = new (p) char[5];
  __builtin_strncpy (q, s, 5);
  return q;

}

void* warn_placement_new_array_strncpy (const char *s)
{
  void *p = __builtin_malloc (4);
  char *q = new (p) char[5];
  __builtin_strncpy (q, s, 5);  // { dg-warning "\\\[-Wstringop-overflow" }
  return q;
}
