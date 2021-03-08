/* PR middle-end/98266 - bogus array subscript is partly outside array
   bounds on virtual inheritance
   Same as Warray-bounds-19.C with nonvirtual inheritance.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void* operator new (__SIZE_TYPE__, void *p) { return p; }
void* operator new[] (__SIZE_TYPE__, void *p) { return p; }


struct A
{
  virtual ~A ();
  int ai;
};

struct B: A { };


// Exercise access to base members by ctor of the most derived class.

struct C1: A
{
  int c1i;
  C1 ();
};

struct D1: B, C1
{
  D1 () { B::ai = 0; C1::ai = 1; c1i = 2; };
};

void sink (void*);

void nowarn_derived_ctor_access_decl ()
{
  D1 d1;
  sink (&d1);
}

void nowarn_derived_ctor_access_new ()
{
  D1 *p = new D1;
  sink (p);
}

void nowarn_derived_ctor_access_placement_new ()
{
  char a[sizeof (D1)];
  D1 *p = new (a) D1;
  sink (p);
}

void nowarn_derived_ctor_access_new_array ()
{
  D1 *p = new D1[2];
  sink (p);
}

void nowarn_derived_ctor_access_placement_new_array ()
{
  char a[sizeof (D1) * 2];
  D1 *p = new (a) D1[2];
  sink (p);
}


// Exercise access to base members by ctor of the second most derived class.

struct C2: A
{
  int c2i;
  ~C2 () { ai = 0; c2i = 1; }
};

struct D2: B, C2
{
  D2 ();
};

void nowarn_base_dtor_access_decl ()
{
  D2 d2;
  sink (&d2);
}

void nowarn_base_dtor_access_new ()
{
  D2 *p = new D2;
  sink (p);
}

void nowarn_base_dtor_access_placement_new ()
{
  char a[sizeof (D2)];
  D2 *p = new (a) D2;
  sink (p);
}

void nowarn_base_dtor_access_new_array ()
{
  D2 *p = new D2[2];
  sink (p);
}

void nowarn_base_dtor_access_placement_new_array ()
{
  char a[sizeof (D2) * 2];
  D2 *p = new (a) D2[2];
  sink (p);
}
