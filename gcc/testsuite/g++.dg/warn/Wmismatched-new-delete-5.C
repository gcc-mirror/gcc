/* PR c++/100876 - -Wmismatched-new-delete should either look through
   or ignore placement new
   { dg-do compile }
   { dg-options "-O2 -Wall -fdelete-null-pointer-checks" } */

extern "C" {
  void* malloc (__SIZE_TYPE__);
  void free (void*);
}

inline void* operator new (__SIZE_TYPE__, void *p) { return p; }
inline void* operator new[] (__SIZE_TYPE__, void *p) { return p; }

void nowarn_placement_new ()
{
  free (new (malloc (sizeof (int))) int ());      // { dg-bogus "-Wmismatched-new-delete" }
}

void nowarn_placement_array_new ()
{
  free (new (malloc (sizeof (int) * 2)) int[2]);  // { dg-bogus "-Wmismatched-new-delete" }
}


void warn_placement_new ()
{
  void *p = malloc (sizeof (int));
  int *q = new (p) int ();
  delete q;                   // { dg-warning "-Wmismatched-new-delete" }
}

void warn_placement_array_new ()
{
  void *p = malloc (sizeof (int));
  int *q = new (p) int[2];
  delete q;                   // { dg-warning "-Wmismatched-new-delete" }
}
