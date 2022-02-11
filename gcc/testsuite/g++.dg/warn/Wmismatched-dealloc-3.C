/* Verify that passing a pointer to a deallocation function that was
   previously passed to a mismatched reallocation function is diagnosed
   by -Wmismatched-dealloc (and not by some other warning).
   { dg-do compile }
   { dg-options "-Wall" } */

#define A(...) __attribute__ ((malloc (__VA_ARGS__)))

typedef __SIZE_TYPE__ size_t;

extern "C"
{
  void free (void *);
  void* realloc (void *, size_t);
}

// User-defined allocator/deallocator just like like realloc.
                   int* int_realloc (size_t, int *);
A (int_realloc, 2) int* int_realloc (size_t, int *);


void sink (void *);


void* warn_realloc_op_delete (void *p)
{
  void *q = realloc (p, 5);   // { dg-message "call to 'void\\* realloc\\(void\\*, size_t\\)'" "note" }

  operator delete (p);        // { dg-warning "'void operator delete\\(void\\*\\)' called on pointer 'p' passed to mismatched allocation function 'void\\* realloc\\(void\\*, size_t\\)' \\\[-Wmismatched-dealloc" }
  return q;
}

void* warn_realloc_op_delete_cond (void *p)
{
  void *q = realloc (p, 5);      // { dg-message "call to 'void\\* realloc\\(void\\*, size_t\\)'" "note" }

  if (!q)
    operator delete (p);         // { dg-warning "'void operator delete\\(void\\*\\)' called on pointer 'p' passed to mismatched allocation function 'void\\* realloc\\(void\\*, size_t\\)'" }
  return q;
}

void* warn_realloc_array_delete_char (char *p)
{
  char *q;
  q = (char*)realloc (p, 7);  // { dg-message "call to 'void\\* realloc\\(void\\*, size_t\\)'" "note" }

  if (!q)
    delete[] (p);             // { dg-warning "'void operator delete \\\[]\\(void\\*\\)' called on pointer 'p' passed to mismatched allocation function 'void\\* realloc\\(void\\*, size_t\\)'" }
  return q;
}


int* warn_int_realloc_op_delete (int *p)
{
  int *q;
  q = int_realloc (5, p);     // { dg-message "call to 'int\\* int_realloc\\(size_t, int\\*\\)'" "note" }

  operator delete (p);        // { dg-warning "'void operator delete\\(void\\*\\)' called on pointer 'p' passed to mismatched allocation function 'int\\* int_realloc\\(size_t, int\\*\\)' \\\[-Wmismatched-dealloc" }
  return q;
}


int* warn_int_realloc_free (int *p)
{
  int *q;
  q = int_realloc (5, p);    // { dg-message "call to 'int\\* int_realloc\\(size_t, int\\*\\)'" "note" }

  free (p);                   // { dg-warning "'void free\\(void\\*\\)' called on pointer 'p' passed to mismatched allocation function 'int\\* int_realloc\\(size_t, int\\*\\)' \\\[-Wmismatched-dealloc" }
  return q;
}
