/* PR middle-end/98266 - bogus array subscript is partly outside array
   bounds on virtual inheritance
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct A
{
  int ai, aj, aa[2];

  virtual ~A ();
};

struct B: virtual A { };
struct C: virtual A { };

void sink (void*);

struct C1: virtual A
{
  int c2i, c2j, c2a[2];

  C1 ();
  ~C1 ()
  {                           // { dg-bogus "\\\[-Warray-bounds" }
    c2i = __LINE__;           // { dg-bogus "\\\[-Warray-bounds" }
    c2j = __LINE__;           // { dg-bogus "\\\[-Warray-bounds" }
    c2a[0] = __LINE__;        // { dg-bogus "\\\[-Warray-bounds" }
    c2a[1] = __LINE__;        // { dg-bogus "\\\[-Warray-bounds" }
    c2a[2] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  }
};

struct D1: virtual B, virtual C1
{
  D1 ();
};

void sink (void*);

/* Verify that only out of bounds accesses to members of an ordinary base
   class are diagnosed.  Use direct array accesses.  */
void test_vmem_base_ctor_arryaccess ()
{
  D1 d2;
  sink (&d2);
}


struct C2: virtual A
{
  int c3a[2];

  C2 ();
  ~C2 ()
  {                           // { dg-bogus "\\\[-Warray-bounds" }
    int *p = c3a;
    *p++ = __LINE__;
    *p++ = __LINE__;
    *p++ = __LINE__;          // { dg-warning "\\\[-Warray-bounds" }
  }
};

struct D2: virtual B, virtual C2
{
  D2 ();
};

/* Verify that only out of bounds accesses to members of an ordinary base
   class are diagnosed.  Use pointer accesses.  */
void test_vmem_base_dtor_ptraccess ()
{
  D2 d3;
  sink (&d3);
}


struct C3: virtual A          // { dg-bogus "\\\[-Warray-bounds" }
{
  int i, j, a[2];

  C3 ();
};

struct D3: virtual B, virtual C3
{
  D3 ()
  {                           // { dg-bogus "\\\[-Warray-bounds" }
    i = __LINE__;             // { dg-bogus "\\\[-Warray-bounds" }
    j = __LINE__;             // { dg-bogus "\\\[-Warray-bounds" }
    a[0] = __LINE__;          // { dg-bogus "\\\[-Warray-bounds" }
    a[1] = __LINE__;          // { dg-bogus "\\\[-Warray-bounds" }
    a[2] = __LINE__;          // { dg-warning "\\\[-Warray-bounds" }
  }
};

/* Verify that only out of bounds accesses to members of an ordinary base
   class made in the ctor of a derived class are diagnosed.  Use direct
   array accesses.  */
void test_vmem_derived_ctor_arryaccess ()
{
  D3 d4;
  sink (&d4);
}


struct D4: virtual B, virtual C3
{
  D4 ()
  {                           // { dg-bogus "\\\[-Warray-bounds" }
    int *p = a;
    *p++ = __LINE__;
    *p++ = __LINE__;
    *p++ = __LINE__;          // { dg-warning "\\\[-Warray-bounds" }
  }
};

/* Verify that only out of bounds accesses to members of an ordinary base
   class made in the ctor of a derived class are diagnosed.  Use pointer
   accesses.  */
void test_vmem_derived_ctor_ptraccess ()
{
  D4 d5;
  sink (&d5);
}


struct D5: virtual B, virtual C3  // { dg-bogus "\\\[-Warray-bounds" }
{
  ~D5 ()
  {
    i = __LINE__;             // { dg-bogus "\\\[-Warray-bounds" }
    j = __LINE__;             // { dg-bogus "\\\[-Warray-bounds" }
    a[0] = __LINE__;          // { dg-bogus "\\\[-Warray-bounds" }
    a[1] = __LINE__;          // { dg-bogus "\\\[-Warray-bounds" }
    a[2] = __LINE__;          // { dg-warning "\\\[-Warray-bounds" }
  }
};

/* Verify that only out of bounds accesses to members of an ordinary base
   class made in the dtor of a derived class are diagnosed.  Use pointer
   accesses.  */
void test_vmem_derived_dtor_arryaccess ()
{
  D5 d6;
  sink (&d6);
}


struct D6: virtual B, virtual C3  // { dg-bogus "\\\[-Warray-bounds" }
{
  ~D6 ()
  {
    int *p = a;
    *p++ = __LINE__;
    *p++ = __LINE__;
    *p++ = __LINE__;          // { dg-warning "\\\[-Warray-bounds" }
  }
};

/* Verify that only out of bounds accesses to members of an ordinary base
   class made in the dtor of a derived class are diagnosed.  Use pointer
   accesses.  */
void test_vmem_derived_dtor_ptraccess ()
{
  D6 d7;
  sink (&d7);
}
