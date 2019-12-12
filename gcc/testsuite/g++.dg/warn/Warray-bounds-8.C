/* PR middle-end/91458 - inconsistent warning for writing past the end
   of an array member
   See Wstringop-overflow-3.C for the same test that exercises the other
   warning.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-stringop-overflow" }
   { dg-skip-if "" { *-*-aix* } } */

void sink (void*);

// Exercise flexible array members.

struct Ax
{
  char n;
  char a[];                     // { dg-message "while referencing .Ax::a." }
};

// Verify warning for a definition with no initializer.
Ax ax_;

void gax_ ()
{
  ax_.a[0] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  ax_.a[1] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  ax_.a[2] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the flexible array member.
Ax ax0 = { 0 };

void gax0 ()
{
  ax0.a[0] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  ax0.a[1] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  ax0.a[2] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for access to a definition with an initializer that
// initializes the flexible array member to empty.
Ax ax0_ = { 0, { } };

void gax0_ ()
{
  ax0_.a[0] = 0;                // { dg-warning "\\\[-Warray-bounds" }
  ax0_.a[1] = 0;                // { dg-warning "\\\[-Warray-bounds" }
  ax0_.a[2] = 0;                // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for out-of-bounds accesses to a definition with
// an initializer.
Ax ax1 = { 1, { 0 } };

void gax1 ()
{
  ax1.a[0] = 0;
  ax1.a[1] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  ax1.a[2] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}

Ax ax2 = { 2, { 1, 0 } };

void gax2 ()
{
  ax2.a[0] = 0;
  ax2.a[1] = 0;
  ax2.a[2] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}


// Verify no warning for an unknown struct object.
void gaxp (Ax *p)
{
  p->a[0] = 0;
  p->a[3] = 0;
  p->a[9] = 0;
}


// Verify no warning for an extern struct object whose array may be
// initialized to any number of elements.
extern Ax axx;

void gaxx ()
{
  axx.a[0] = 0;
  axx.a[3] = 0;
  axx.a[9] = 0;
}

// Exercise zero-length array members.

struct A0
{
  char n;
  char a[0];                    // { dg-message "while referencing .A0::a." }
};

// Verify warning for a definition with no initializer.
A0 a0_;

void ga0_ ()
{
  a0_.a[0] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  a0_.a[1] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  a0_.a[2] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the flexible array member.
A0 a00 = { 0 };

void ga00 ()
{
  a00.a[0] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  a00.a[1] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  a00.a[2] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for access to a definition with an initializer that
// initializes the flexible array member to empty.
A0 a00_ = { 0, { } };

void ga00_ ()
{
  a00_.a[0] = 0;                // { dg-warning "\\\[-Warray-bounds" }
  a00_.a[1] = 0;                // { dg-warning "\\\[-Warray-bounds" }
  a00_.a[2] = 0;                // { dg-warning "\\\[-Warray-bounds" }
}

// The following are rejected with
//   error: too many initializers for 'char [0]'
// A0 a01 = { 1, { 0 } };
// A0 a02 = { 2, { 1, 0 } };


// Verify no warning for an unknown struct object.
void ga0p (A0 *p)
{
  p->a[0] = 0;
  p->a[3] = 0;
  p->a[9] = 0;
}


// Verify warning for an extern struct object which (unlike a true
// flexible array member) may not be initialized.
extern A0 a0x;

void ga0x ()
{
  a0x.a[0] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  a0x.a[3] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  a0x.a[9] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}


// Exercise trailing one-element array members.

struct A1
{
  char n;
  char a[1];                    // { dg-message "while referencing .A1::a." }
};

// Verify warning for a definition with no initializer.
A1 a1_;

void ga1_ ()
{
  a1_.a[0] = 0;
  a1_.a[1] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  a1_.a[2] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the one-element array member.
A1 a1__ = { 0 };

void ga1__ ()
{
  a1__.a[0] = 0;
  a1__.a[1] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  a1__.a[2] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member to empty.
A1 a1_0 = { 0, { } };

void ga1_0_ ()
{
  a1_0.a[0] = 0;
  a1_0.a[1] = 0;                // { dg-warning "\\\[-Warray-bounds" }
  a1_0.a[2] = 0;                // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member.
A1 a1_1 = { 0, { 1 } };

void ga1_1 ()
{
  a1_1.a[0] = 0;
  a1_1.a[1] = 0;                // { dg-warning "\\\[-Warray-bounds" }
  a1_1.a[2] = 0;                // { dg-warning "\\\[-Warray-bounds" }
}


// Verify no warning for an unknown struct object.
void ga1p (A1 *p)
{
  p->a[0] = 0;
  p->a[3] = 0;
  p->a[9] = 0;
}


// Verify warning for an extern struct object.  Similar to the zero-length
// array case, a one-element trailing array can be initialized to at most
// a single element.
extern A1 a1x;

void ga1x ()
{
  a1x.a[0] = 0;
  a1x.a[3] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  a1x.a[9] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}

// Exercise interior one-element array members (verify they're not
// treated as trailing.

struct A1i
{
  char n;
  char a[1];                    // { dg-message "while referencing .A1i::a." }
  char x;
};

// Verify warning for a definition with no initializer.
A1i a1i_;

void ga1i_ ()
{
  a1i_.a[0] = 0;
  a1i_.a[1] = 0;                // { dg-warning "\\\[-Warray-bounds" }
  a1i_.a[2] = 0;                // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the one-element array member.
A1i a1i__ = { 0 };

void ga1i__ ()
{
  a1i__.a[0] = 0;
  a1i__.a[1] = 0;                // { dg-warning "\\\[-Warray-bounds" }
  a1i__.a[2] = 0;                // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member to empty.
A1 a1i_0 = { 0, { } };

void ga1i_0_ ()
{
  a1i_0.a[0] = 0;
  a1i_0.a[1] = 0;               // { dg-warning "\\\[-Warray-bounds" }
  a1i_0.a[2] = 0;               // { dg-warning "\\\[-Warray-bounds" }
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member.
A1 a1i_1 = { 0, { 1 } };

void ga1i_1 ()
{
  a1i_1.a[0] = 0;
  a1i_1.a[1] = 0;               // { dg-warning "\\\[-Warray-bounds" }
  a1i_1.a[2] = 0;               // { dg-warning "\\\[-Warray-bounds" }
}


// Verify no warning for an unknown struct object.
void ga1ip (A1i *p)
{
  p->a[0] = 0;
  p->a[3] = 0;                  // { dg-warning "\\\[-Warray-bounds" }
  p->a[9] = 0;                  // { dg-warning "\\\[-Warray-bounds" }
}


// Verify no warning for an extern struct object.
extern A1i a1ix;

void ga1ix ()
{
  a1ix.a[0] = 0;
  a1ix.a[3] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
  a1ix.a[9] = 0;                 // { dg-warning "\\\[-Warray-bounds" }
}


// Verify non-POD classes with flexible array members.

struct Bx
{
  char n;
  char a[];                     // { dg-message "while referencing .Bx::a." }

  // Verify the warning for a constant.
  Bx () { a[0] = 0; }           // { dg-warning "\\\[-Warray-bounds" }

  // And also for a non-constant.  Regardless of the subscript, the array
  // of the object in function gxi() below has a zero size.
  Bx (int i) { a[i] = 0; }      // { dg-warning "\\\[-Warray-bounds" }
};

void gbx (void)
{
  struct Bx bx;
  sink (&bx);
}

void gbxi (int i)
{
  struct Bx bxi (i);
  sink (&bxi);
}

struct B0
{
  char n;
  char a[0];                    // { dg-message "while referencing .B0::a." }

  B0 () { a[0] = 0; }           // { dg-warning "\\\[-Warray-bounds" }
};


void gb0 (void)
{
  struct B0 b0;
  sink (&b0);
}


struct B1
{
  char n;
  char a[1];                    // { dg-message "while referencing .B1::a." }

  B1 () { a[1] = 0; }           // { dg-warning "\\\[-Warray-bounds" }
};

void gb1 (void)
{
  struct B1 b1;
  sink (&b1);
}


struct B123
{
  char a[123];                  // { dg-message "while referencing .B123::a." }

  B123 () { a[123] = 0; }       // { dg-warning "\\\[-Warray-bounds" }
};

void gb123 (void)
{
  struct B123 b123;
  sink (&b123);
}


struct B234
{
  char a[234];                  // { dg-message "while referencing .B234::a." }

  B234 (int i) { a[i] = 0; }    // { dg-warning "\\\[-Warray-bounds" }
};

void g234 (void)
{
  struct B234 b234 (234);
  sink (&b234);
}
