/* PR middle-end/94527 - Add an attribute that marks a function as freeing
   an object
   The detection doesn't require optimization.
   { dg-do compile }
   { dg-options "-Wall" } */

#define A(...) __attribute__ ((malloc (__VA_ARGS__)))

typedef __SIZE_TYPE__ size_t;

extern "C" {
  void free (void *);
  void* realloc (void *, size_t);
}

void sink (void *);

void                   mydealloc (int, void*);
void* A (mydealloc, 2) myalloc (void*);


void my_delete (const char*, void*);
void my_array_delete (const char*, void*);

typedef void OpDelete1 (void*);
typedef void OpDelete2 (void*, size_t);

A ((OpDelete1*)operator delete, 1)
#if __cplusplus >= 201402L
A ((OpDelete2*)operator delete, 1)
#endif
A (my_delete, 2)
int* my_new (size_t);

A ((OpDelete1*)operator delete[], 1)
#if __cplusplus >= 201402L
A ((OpDelete2*)operator delete[], 1)
#endif
A (my_array_delete, 2)
int* my_array_new (size_t);


void test_my_new ()
{
  {
    void *p = my_new (1);
    operator delete (p);
  }
  {
    void *p = my_new (1);
    sink (p);
    operator delete (p);
  }
  {
    int *p = my_new (1);
    sink (p);
    delete p;
  }

  {
    void *p = my_new (1);
    // { dg-message "returned from 'int\\\* my_new\\\(size_t\\\)'" "note" { target *-*-* } .-1 }
    operator delete[] (p);
    // { dg-warning "'void operator delete \\\[]\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function \\\[-Wmismatched-new-delete" "" { target *-*-* } .-1 }
  }
  {
    void *p = my_new (1);
    // { dg-message "returned from 'int\\\* my_new\\\(size_t\\\)'" "note" { target *-*-* } .-1 }
    sink (p);
    operator delete[] (p);
    // { dg-warning "'void operator delete \\\[]\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function \\\[-Wmismatched-new-delete" "" { target *-*-* } .-1 }
  }
  {
    int *p = my_new (1);
    sink (p);
    delete[] p;
    // { dg-warning "'void operator delete \\\[]\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function \\\[-Wmismatched-new-delete" "" { target *-*-* } .-1 }
  }

  {
    void *p = my_new (1);
    my_delete ("1", p);
  }
  {
    void *p = my_new (1);
    sink (p);
    my_delete ("2", p);
  }

  {
    void *p = my_new (1);
    // { dg-message "returned from 'int\\\* my_new\\\(size_t\\\)'" "note" { target *-*-* } .-1 }
    sink (p);
    my_array_delete ("3", p);
    // { dg-warning "'void my_array_delete\\\(const char\\\*, void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }

  {
    void *p = my_new (1);
    // { dg-message "returned from 'int\\\* my_new\\\(size_t\\\)'" "note" { target *-*-* } .-1 }
    sink (p);
    free (p);
    // { dg-warning "'void free\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }

  {
    void *p = my_new (1);
    // { dg-message "returned from 'int\\\* my_new\\\(size_t\\\)'" "note" { target *-*-* } .-1 }
    sink (p);
    p = realloc (p, 123);
    // { dg-warning "'void\\\* realloc\\\(void\\\*, size_t\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }
}


void test_my_array_new ()
{
  {
    void *p = my_array_new (1);
    operator delete[] (p);
  }
  {
    void *p = my_array_new (1);
    sink (p);
    operator delete[] (p);
  }
  {
    int *p = my_array_new (1);
    sink (p);
    delete[] p;
  }

  {
    void *p = my_array_new (1);
    // { dg-message "returned from 'int\\\* my_array_new\\\(size_t\\\)'" "note" { target *-*-* } .-1 }
    operator delete (p);
    // { dg-warning "'void operator delete\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function \\\[-Wmismatched-new-delete" "" { target *-*-* } .-1 }
  }
  {
    void *p = my_array_new (1);
    // { dg-message "returned from 'int\\\* my_array_new\\\(size_t\\\)'" "note" { target *-*-* } .-1 }
    sink (p);
    operator delete (p);
    // { dg-warning "'void operator delete\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function \\\[-Wmismatched-new-delete" "" { target *-*-* } .-1 }
  }
  {
    int *p = my_array_new (1);
    sink (p);
    delete p;
    // { dg-warning "'void operator delete\\\(void\\\*\[^\)\]*\\\)' called on pointer returned from a mismatched allocation function \\\[-Wmismatched-new-delete" "" { target *-*-* } .-1 }
  }

  {
    void *p = my_array_new (1);
    my_array_delete ("1", p);
  }
  {
    void *p = my_array_new (1);
    sink (p);
    my_array_delete ("2", p);
  }
  {
    void *p = my_array_new (1);
    // { dg-message "returned from 'int\\\* my_array_new\\\(size_t\\\)'" "note" { target *-*-* } .-1 }
    sink (p);
    my_delete ("3", p);
    // { dg-warning "'void my_delete\\\(const char\\\*, void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }

  {
    void *p = my_array_new (1);
    // { dg-message "returned from 'int\\\* my_array_new\\\(size_t\\\)'" "note" { target *-*-* } .-1 }
    sink (p);
    free (p);
    // { dg-warning "'void free\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }

  {
    void *p = my_array_new (1);
    // { dg-message "returned from 'int\\\* my_array_new\\\(size_t\\\)'" "note" { target *-*-* } .-1 }
    sink (p);
    p = realloc (p, 123);
    // { dg-warning "'void\\\* realloc\\\(void\\\*, size_t\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }
}
