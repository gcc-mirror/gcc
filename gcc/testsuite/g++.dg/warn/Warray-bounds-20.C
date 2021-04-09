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
    ci = 0;                   // { dg-warning "\\\[-Warray-bounds" "assign" { xfail *-*-* } }
  }
};

void sink (void*);

void warn_derived_ctor_access_new_decl ()
{
  char a[sizeof (D1)];        // { dg-message "referencing 'a'" "note" }
  char *p = a;
  ++p;
  D1 *q = new (p) D1;
  sink (q);
}

void warn_derived_ctor_access_new_alloc ()
{
  char *p = (char*)operator new (sizeof (D1));    // { dg-message "referencing an object of size \\d+ allocated by 'void\\\* operator new\\\(" "note" }
  ++p;
  D1 *q = new (p) D1;
  sink (q);
}

void warn_derived_ctor_access_new_array_decl ()
{
  char b[sizeof (D1) * 2];    // { dg-message "referencing 'b'" "note" }
  char *p = b;
  ++p;
  D1 *q = new (p) D1[2];
  sink (q);
}

void warn_derived_ctor_access_new_array_alloc ()
{
  char *p = new char[sizeof (D1) * 2];            // { dg-message "referencing an object of size \\d+ allocated by 'void\\\* operator new \\\[]\\\(" "note" }
  ++p;
  D1 *q = new (p) D1[2];
  sink (q);
}
