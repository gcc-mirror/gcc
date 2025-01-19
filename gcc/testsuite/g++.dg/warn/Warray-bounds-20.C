/* PR middle-end/98266 - bogus array subscript is partly outside array
   bounds on virtual inheritance
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void* operator new (__SIZE_TYPE__, void *p) { return p; }
void* operator new[] (__SIZE_TYPE__, void *p) { return p; }


struct A
{
  int ai;
  virtual ~A ();
};

struct B: virtual A { };

struct C: virtual A
{
  int ci;
  C ();
};

struct D1: virtual B, virtual C
{
  /* The warning would ideally point to the assignment but instead points
     to the opening brace.  */
  D1 ()
  {                           // { dg-warning "\\\[-Warray-bounds" "brace" }
    ci = 0;                   // { dg-warning "\\\[-Warray-bounds" "assign" { xfail lp64 } }
  }
};

void sink (void*);

void warn_derived_ctor_access_new_decl ()
{
  char a[sizeof (D1)];        // { dg-message "at offset 1 into object 'a' of size 40" "LP64 note" { target lp64} }
                              // { dg-message "at offset 1 into object 'a' of size 20" "LP64 note" { target ilp32} .-1 }
  char *p = a;
  ++p;
  D1 *q = new (p) D1;
  sink (q);
}

void warn_derived_ctor_access_new_alloc ()
{
  char *p = (char*)operator new (sizeof (D1));    // { dg-message "at offset 1 into object of size \\d+ allocated by '\[^\n\r]*operator new\[^\n\r]*'" "note" }
  ++p;
  D1 *q = new (p) D1;
  sink (q);
}

void warn_derived_ctor_access_new_array_decl ()
{
  char b[sizeof (D1) * 2];    // { dg-message "at offset \\d+ into object 'b' of size 80" "LP64 note" { target { lp64 } xfail { lp64 } } }
                              // { dg-message "at offset \\d+ into object 'b' of size 40" "LP64 note" { target { ilp32 } xfail { ilp32 } } .-1 }
  char *p = b;
  ++p;
  D1 *q = new (p) D1[2];
  sink (q);
}

void warn_derived_ctor_access_new_array_alloc ()
{
  char *p = new char[sizeof (D1) * 2];            // { dg-message "at offset \\d+ into object of size \\d+ allocated by '\[^\n\r]*operator new\[^\n\r]*" "note" { xfail *-*-* } }
  ++p;
  D1 *q = new (p) D1[2];
  sink (q);
}
