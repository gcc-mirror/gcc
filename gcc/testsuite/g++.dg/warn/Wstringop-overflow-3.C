/* PR middle-end/91458 - inconsistent warning for writing past the end
   of an array member
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds" } */

#define NOIPA __attribute__ ((noipa))

void sink (void*);

// Exercise flexible array members.

struct Ax
{
  char n;
  char a[];                     // { dg-message "at offset \[0-2\] to object 'Ax::a' declared here" "note: flexarray" }
};

// Verify warning for a definition with no initializer.
Ax ax_;

NOIPA void gax_ ()
{
  ax_.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  ax_.a[1] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  ax_.a[2] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the flexible array member.
Ax ax0 = { 0 };

NOIPA void gax0 ()
{
  ax0.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  ax0.a[1] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  ax0.a[2] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that
// initializes the flexible array member to empty.
Ax ax0_ = { 0, { } };

NOIPA void gax0_ ()
{
  ax0_.a[0] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
  ax0_.a[1] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
  ax0_.a[2] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for out-of-bounds accesses to a definition with
// an initializer.
Ax ax1 = { 1, { 0 } };

NOIPA void gax1 ()
{
  ax1.a[0] = 0;
  ax1.a[1] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  ax1.a[2] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

Ax ax2 = { 2, { 1, 0 } };

NOIPA void gax2 ()
{
  ax2.a[0] = 0;
  ax2.a[1] = 0;
  ax2.a[2] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}


// Verify no warning for an unknown struct object.
NOIPA void gaxp (Ax *p)
{
  p->a[0] = 0;
  p->a[3] = 0;
  p->a[9] = 0;
}


// Verify no warning for an extern struct object whose array may be
// initialized to any number of elements.
extern Ax axx;

NOIPA void gaxx ()
{
  axx.a[0] = 0;
  axx.a[3] = 0;
  axx.a[9] = 0;
}

// Exercise zero-length array members.

struct A0
{
  char n;
  char a[0];                    // { dg-message "at offset \[0-2\] to object 'A0::a' with size 0 declared here" "note: trailing zero-length array" }
};

// Verify warning for a definition with no initializer.
A0 a0_;

NOIPA void ga0_ ()
{
  a0_.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a0_.a[1] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a0_.a[2] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the flexible array member.
A0 a00 = { 0 };

NOIPA void ga00 ()
{
  a00.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a00.a[1] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a00.a[2] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that
// initializes the flexible array member to empty.
A0 a00_ = { 0, { } };

NOIPA void ga00_ ()
{
  a00_.a[0] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
  a00_.a[1] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
  a00_.a[2] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
}

// The following are rejected with
//   error: too many initializers for 'char [0]'
// A0 a01 = { 1, { 0 } };
// A0 a02 = { 2, { 1, 0 } };


// Verify no warning for an unknown struct object.
NOIPA void ga0p (A0 *p)
{
  p->a[0] = 0;
  p->a[3] = 0;
  p->a[9] = 0;
}


// Verify warning for an extern struct object which (unlike a true
// flexible array member) may not be initialized.
extern A0 a0x;

NOIPA void ga0x ()
{
  a0x.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a0x.a[3] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a0x.a[9] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}


// Exercise trailing one-element array members.

struct A1
{
  char n;
  char a[1];                    // { dg-message "at offset \[1-9\] to object 'A1::a' with size 1 declared here" "note: trailing one-element array" }
};

// Verify warning for a definition with no initializer.
A1 a1_;

NOIPA void ga1_ ()
{
  a1_.a[0] = 0;
  a1_.a[1] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a1_.a[2] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the one-element array member.
A1 a1__ = { 0 };

NOIPA void ga1__ ()
{
  a1__.a[0] = 0;
  a1__.a[1] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a1__.a[2] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member to empty.
A1 a1_0 = { 0, { } };

NOIPA void ga1_0_ ()
{
  a1_0.a[0] = 0;
  a1_0.a[1] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
  a1_0.a[2] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member.
A1 a1_1 = { 0, { 1 } };

NOIPA void ga1_1 ()
{
  a1_1.a[0] = 0;
  a1_1.a[1] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
  a1_1.a[2] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
}


// Verify no warning for an unknown struct object.
NOIPA void ga1p (A1 *p)
{
  p->a[0] = 0;
  p->a[3] = 0;
  p->a[9] = 0;
}


// Verify warning for an extern struct object.  Similar to the zero-length
// array case, a one-element trailing array can be initialized to at most
// a single element.
extern A1 a1x;

NOIPA void ga1x ()
{
  a1x.a[0] = 0;
  a1x.a[3] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a1x.a[9] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Exercise interior one-element array members (verify they're not
// treated as trailing.

struct A1i
{
  char n;
  char a[1];                    // { dg-message "at offset \[1-9\] to object 'A1i::a' with size 1 declared here" "note: interior one-element array" }
  char x;
};

// Verify warning for a definition with no initializer.
A1i a1i_;

NOIPA void ga1i_ ()
{
  a1i_.a[0] = 0;
  a1i_.a[1] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
  a1i_.a[2] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the one-element array member.
A1i a1i__ = { 0 };

NOIPA void ga1i__ ()
{
  a1i__.a[0] = 0;
  a1i__.a[1] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
  a1i__.a[2] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member to empty.
A1 a1i_0 = { 0, { } };

NOIPA void ga1i_0_ ()
{
  a1i_0.a[0] = 0;
  a1i_0.a[1] = 0;               // { dg-warning "\\\[-Wstringop-overflow" }
  a1i_0.a[2] = 0;               // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member.
A1 a1i_1 = { 0, { 1 } };

NOIPA void ga1i_1 ()
{
  a1i_1.a[0] = 0;
  a1i_1.a[1] = 0;               // { dg-warning "\\\[-Wstringop-overflow" }
  a1i_1.a[2] = 0;               // { dg-warning "\\\[-Wstringop-overflow" }
}


// Verify no warning for an unknown struct object.
NOIPA void ga1ip (A1i *p)
{
  p->a[0] = 0;
  p->a[3] = 0;                  // { dg-warning "\\\[-Wstringop-overflow" }
  p->a[9] = 0;                  // { dg-warning "\\\[-Wstringop-overflow" }
}


// Verify no warning for an extern struct object.
extern A1i a1ix;

NOIPA void ga1ix ()
{
  a1ix.a[0] = 0;
  a1ix.a[3] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a1ix.a[9] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}


// Verify non-POD classes with flexible array members.

struct Bx
{
  char n;
  char a[];                     // { dg-message "at offset 0 to object 'Bx::a' declared here" "note: flexarray class member" }

  // Verify the warning for a constant.
  Bx () { a[0] = 0; }           // { dg-warning "\\\[-Wstringop-overflow" }

  // And also for a non-constant.  Regardless of the subscript, the array
  // of the object in function gxi() below has a zero size.
  Bx (int i) { a[i] = 0; }      // { dg-warning "\\\[-Wstringop-overflow" }
};

NOIPA void gbx (void)
{
  struct Bx bx;
  sink (&bx);
}

NOIPA void gbxi (int i)
{
  struct Bx bxi (i);
  sink (&bxi);
}

struct B0
{
  char n;
  char a[0];                    // { dg-message "at offset 0 to object 'B0::a' with size 0 declared here" "note: zero-length trailing array class member" }

  B0 () { a[0] = 0; }           // { dg-warning "\\\[-Wstringop-overflow" }
};


NOIPA void gb0 (void)
{
  struct B0 b0;
  sink (&b0);
}


struct B1
{
  char n;
  char a[1];                    // { dg-message "at offset 1 to object 'B1::a' with size 1 declared here" "note: one-element trailing array class member" }

  B1 () { a[1] = 0; }           // { dg-warning "\\\[-Wstringop-overflow" }
};

NOIPA void gb1 (void)
{
  struct B1 b1;
  sink (&b1);
}


struct B123
{
  char a[123];                  // { dg-message "at offset 123 to object 'B123::a' with size 123 declared here" "note: large trailing array class member" }

  B123 () { a[123] = 0; }       // { dg-warning "\\\[-Wstringop-overflow" }
};

NOIPA void gb123 (void)
{
  struct B123 b123;
  sink (&b123);
}


struct B234
{
  char a[234];                  // { dg-message "at offset 234 to object 'B234::a' with size 234 declared here" "note: large trailing array class member" }

  B234 (int i) { a[i] = 0; }    // { dg-warning "\\\[-Wstringop-overflow" }
};

NOIPA void g234 (void)
{
  struct B234 b234 (234);
  sink (&b234);
}
