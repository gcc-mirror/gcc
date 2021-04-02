/* PR c++/90629 - Support for -Wmismatched-new-delete
   The detection doesn't require optimization.
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __SIZE_TYPE__ size_t;

extern "C" {
  void free (void *);
  void* malloc (size_t);
  void* realloc (void *, size_t);
  char* strdup (const char *);
  char* strndup (const char *, size_t);
}

void sink (void *);

void nowarn_op_new_delete (int n)
{
  void *p = operator new (n);
  sink (p);
  operator delete (p);
}

void nowarn_new_delete (int n)
{
  {
    char *p = new char;
    sink (p);
    delete p;
  }

  {
    char *p = new char[n];
    sink (p);
    delete[] p;
  }
}

/* Verify a warning for calls to free() with a pointer returned from
   a call to operator new() or the new expressopm.  */

void warn_new_free (int n)
{
  {
    void *p = operator new (n);
    // { dg-message "returned from 'void\\\* operator new\\\(" "note" { target *-*-* } .-1 }
    sink (p);
    free (p);
    // { dg-warning "'void free\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }
  {
    char *p = new char[n];
    // { dg-message "returned from 'void\\\* operator new \\\[" "note" { target *-*-* } .-1 }
    sink (p);
    free (p);
    // { dg-warning "'void free\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }
}


/* Verify a warning for calls to realloc() with a pointer returned from
   a call to operator new() or the new expressopm.  */

void warn_new_realloc (int n)
{
  {
    void *p = operator new (n);
    // { dg-message "returned from 'void\\\* operator new\\\(" "note" { target *-*-* } .-1 }
    sink (p);
    p = realloc (p, n * 2);
    // { dg-warning "'void\\\* realloc\\\(\[^)\]+\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
    sink (p);
  }
  {
    void *p = new char[n];
    // { dg-message "returned from 'void\\\* operator new \\\[" "note" { target *-*-* } .-1 }
    sink (p);
    p = realloc (p, n * 2);
    // { dg-warning "'void\\\* realloc\\\(\[^)\]+\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
    sink (p);
  }
}


/* Verify a warning for a call to operator_delete() with a pointer returned
   from a call to malloc().  */

void warn_malloc_op_delete (int n)
{
  char *p = (char *)malloc (n);
  // { dg-message "returned from 'void\\\* malloc\\\(" "note" { target *-*-* } .-1 }
  sink (p);
  operator delete (p);
  // { dg-warning "'void operator delete\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
}


/* Verify a warning for an invocation of either form of the delete
   expression with a pointer returned from malloc().  */

void warn_malloc_delete (int n)
{
  {
    char *p = (char *)malloc (n);
    // { dg-message "returned from 'void\\\* malloc\\\(" "note" { target *-*-* } .-1 }
    sink (p);
    /* C++98 calls operator delete (void*) but later versions call
       operator delete (void*, size_t).  The difference doesn't matter
       here so verify just that some operator delete is called.  */
    delete p;
    // { dg-warning "'void operator delete\\\(\[^)\]+\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }

  {
    char *p = (char *)malloc (n);
    // { dg-message "returned from 'void\\\* malloc\\\(" "note" { target *-*-* } .-1 }
    sink (p);
    delete[] p;
    // { dg-warning "'void operator delete \\\[]\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }
}


/* Verify a warning for an invocation of either form of the delete
   expression with a pointer returned from realloc().  */

void warn_realloc_delete (void *p1, void *p2, int n)
{
  {
    char *q = (char *)realloc (p1, n);
    // { dg-message "returned from 'void\\\* realloc\\\(" "note" { target *-*-* } .-1 }
    sink (q);
    /* C++98 calls operator delete (void*) but later versions call
       operator delete (void*, size_t).  The difference doesn't matter
       here so verify just that some operator delete is called.  */
    delete q;
    // { dg-warning "'void operator delete\\\(\[^)\]+\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }

  {
    char *q = (char *)realloc (p2, n);
    // { dg-message "returned from 'void\\\* realloc\\\(" "note" { target *-*-* } .-1 }
    sink (q);
    delete[] q;
    // { dg-warning "'void operator delete \\\[]\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }
}


/* Verify a warning for an invocation of either form of the delete
   expression with a pointer returned from strdup().  */

void warn_strdup_delete (const char *s1, const char *s2)
{
  {
    char *q = strdup (s1);
    // { dg-message "returned from 'char\\\* strdup\\\(" "note" { target *-*-* } .-1 }
    sink (q);
    /* C++98 calls operator delete (void*) but later versions call
       operator delete (void*, size_t).  The difference doesn't matter
       here so verify just that some operator delete is called.  */
    delete q;
    // { dg-warning "'void operator delete\\\(\[^)\]+\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }

  {
    char *q = strdup (s2);
    // { dg-message "returned from 'char\\\* strdup\\\(" "note" { target *-*-* } .-1 }
    sink (q);
    delete[] q;
    // { dg-warning "'void operator delete \\\[]\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }
}



/* Verify a warning for an invocation of either form of the delete
   expression with a pointer returned from strndup().  */

void warn_strdup_delete (const char *s1, const char *s2, size_t n)
{
  {
    char *q = strndup (s1, n);
    // { dg-message "returned from 'char\\\* strndup\\\(" "note" { target *-*-* } .-1 }
    sink (q);
    /* C++98 calls operator delete (void*) but later versions call
       operator delete (void*, size_t).  The difference doesn't matter
       here so verify just that some operator delete is called.  */
    delete q;
    // { dg-warning "'void operator delete\\\(\[^)\]+\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }

  {
    char *q = strndup (s2, n);
    // { dg-message "returned from 'char\\\* strndup\\\(" "note" { target *-*-* } .-1 }
    sink (q);
    delete[] q;
    // { dg-warning "'void operator delete \\\[]\\\(void\\\*\\\)' called on pointer returned from a mismatched allocation function" "" { target *-*-* } .-1 }
  }
}


struct Base { virtual ~Base (); };
struct Derived: Base { };

void warn_new_free_base_derived ()
{
  Base *p = new Derived ();
  sink (p);
  free (p);                   // { dg-warning "\\\[-Wmismatched-new-delete" }
}
