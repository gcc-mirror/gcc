/* PR middle-end/91458 - inconsistent warning for writing past the end
   of an array member
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds -fno-ipa-icf" } */

void sink (void*);

// Exercise flexible array members.

struct Ax
{
  char n;
  char a[];                     // { dg-message "destination object" "note" }
};

// Verify warning for a definition with no initializer.
struct Ax ax_;

void gax_ (void)
{
  ax_.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  ax_.a[1] = 1;                 // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  ax_.a[2] = 2;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the flexible array member.
struct Ax ax0 = { 0 };

void gax0 (void)
{
  ax0.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  ax0.a[1] = 1;                 // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  ax0.a[2] = 2;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that
// initializes the flexible array member to empty.
struct Ax ax0_ = { 0, { } };

void gax0_ (void)
{
  ax0_.a[0] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
  ax0_.a[1] = 1;                // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  ax0_.a[2] = 2;                // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for out-of-bounds accesses to a definition with
// an initializer.
struct Ax ax1 = { 1, { 0 } };

void gax1 (void)
{
  ax1.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" "" { target { vect_slp_v2qi_store_unalign } } }
  ax1.a[1] = 1;                 // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  ax1.a[2] = 2;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

struct Ax ax2 = { 2, { 1, 0 } };

void gax2 (void)
{
  ax2.a[0] = 0;
  ax2.a[1] = 1;
  ax2.a[2] = 2;                 // { dg-warning "\\\[-Wstringop-overflow" }
}


// Verify no warning for an unknown struct object.
void gaxp (struct Ax *p)
{
  p->a[0] = 0;
  p->a[3] = 3;
  p->a[9] = 9;
}


// Verify no warning for an extern struct object whose array may be
// initialized to any number of elements.
extern struct Ax axx;

void gaxx (void)
{
  axx.a[0] = 0;
  axx.a[3] = 3;
  axx.a[9] = 9;
}

// Exercise zero-length array members.

struct A0
{
  char n;
  char a[0];                    // { dg-message "destination object" "note" }
};

// Verify warning for a definition with no initializer.
struct A0 a0_;

void ga0_ (void)
{
  a0_.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a0_.a[1] = 1;                 // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a0_.a[2] = 2;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the flexible array member.
struct A0 a00 = { 0 };

void ga00 (void)
{
  a00.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a00.a[1] = 1;                 // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a00.a[2] = 2;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Verify warning for access to a definition with an initializer that
// initializes the flexible array member to empty.
struct A0 a00_ = { 0, { } };

void ga00_ (void)
{
  a00_.a[0] = 0;                // { dg-warning "\\\[-Wstringop-overflow" }
  a00_.a[1] = 1;                // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a00_.a[2] = 2;                // { dg-warning "\\\[-Wstringop-overflow" }
}

// The following are rejected with
//   error: too many initializers for 'char [0]'
// A0 a01 = { 1, { 0 } };
// A0 a02 = { 2, { 1, 0 } };


// Verify no warning for an unknown struct object.
void ga0p (struct A0 *p)
{
  p->a[0] = 0;
  p->a[3] = 3;
  p->a[9] = 9;
}


// Verify warning for an extern struct object which (unlike a true
// flexible array member) may not be initialized.
extern struct A0 a0x;

void ga0x (void)
{
  a0x.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a0x.a[3] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a0x.a[9] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" }
}


// Exercise trailing one-element array members.

struct A1
{
  char n;
  char a[1];                    // { dg-message "destination object" "note" }
};

// Verify warning for a definition with no initializer.
struct A1 a1_;

void ga1_ (void)
{
  a1_.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" "" { target { vect_slp_v2qi_store_unalign } } }
  a1_.a[1] = 1;                 // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a1_.a[2] = 2;                 // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a;
  a.a[0] = 0;                   // { dg-warning "\\\[-Wstringop-overflow" "" { target { vect_slp_v2qi_store_unalign } } }
  a.a[1] = 1;                   // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a.a[2] = 2;                   // { dg-warning "\\\[-Wstringop-overflow" }
  sink (&a);
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the one-element array member.
struct A1 a1__ = { 0 };

void ga1__ (void)
{
  a1__.a[0] = 0;                 // { dg-warning "\\\[-Wstringop-overflow" "" { target { vect_slp_v2qi_store_unalign } } }
  a1__.a[1] = 1;                 // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a1__.a[2] = 2;                 // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a = { 1 };
  a.a[0] = 0;
  a.a[1] = 1;                    // { dg-warning "\\\[-Wstringop-overflow" }
  a.a[2] = 2;                    // { dg-warning "\\\[-Wstringop-overflow" "pr102462" { xfail { vect_slp_v2qi_store_align } } }
  sink (&a);
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member to empty.
struct A1 a1_0 = { 0, { } };

void ga1_0_ (void)
{
  a1_0.a[0] = 0;                // { dg-warning "\\\[-Wstringop-overflow" "" { target { vect_slp_v2qi_store_unalign } } }
  a1_0.a[1] = 1;                // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a1_0.a[2] = 2;                // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a = { 1, { } };
  a.a[0] = 0;
  a.a[1] = 1;                   // { dg-warning "\\\[-Wstringop-overflow" }
  a.a[2] = 2;                   // { dg-warning "\\\[-Wstringop-overflow" "pr102462" { xfail { vect_slp_v2qi_store_align } } }
  sink (&a);
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member.
struct A1 a1_1 = { 0, { 1 } };

void ga1_1 (void)
{
  a1_1.a[0] = 0;                // { dg-warning "\\\[-Wstringop-overflow" "" { target { vect_slp_v2qi_store_unalign } } }
  a1_1.a[1] = 1;                // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a1_1.a[2] = 2;                // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a = { 0, { 1 } };   // { dg-warning "\\\[-Wstringop-overflow" "pr102706" { target { vect_slp_v4qi_store_align } } }
  a.a[0] = 0;
  a.a[1] = 1;                   // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v4qi_store_align } } }
  a.a[2] = 2;                   // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v4qi_store_align } } }
  sink (&a);
}


// Verify no warning for an unknown struct object.
void ga1p (struct A1 *p)
{
  p->a[0] = 0;
  p->a[3] = 3;
  p->a[9] = 9;
}


// Verify warning for an extern struct object.  Similar to the zero-length
// array case, a one-element trailing array can be initialized to at most
// a single element.
extern struct A1 a1x;

void ga1x (void)
{
  a1x.a[0] = 0;
  a1x.a[3] = 3;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a1x.a[9] = 9;                 // { dg-warning "\\\[-Wstringop-overflow" }
}

// Exercise interior one-element array members (verify they're not
// treated as trailing.

struct A1i
{
  char n;
  char a[1];                    // { dg-message "destination object" }
  char x;
};

// Verify warning for a definition with no initializer.
struct A1i a1i_;

void ga1i_ (void)
{
  a1i_.a[0] = 0;
  a1i_.a[1] = 1;                // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a1i_.a[2] = 2;                // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1i a;
  a.a[0] = 1;
  a.a[1] = 2;                   // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a.a[2] = 3;                   // { dg-warning "\\\[-Wstringop-overflow" }
  sink (&a);
}

// Verify warning for access to a definition with an initializer that doesn't
// initialize the one-element array member.
struct A1i a1i__ = { 0 };

void ga1i__ (void)
{
  a1i__.a[0] = 0;
  a1i__.a[1] = 1;                // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a1i__.a[2] = 2;                // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1i a = { 0 };
  a.a[0] = 0;
  a.a[1] = 1;                    // { dg-warning "\\\[-Wstringop-overflow" }
  a.a[2] = 2;                    // { dg-warning "\\\[-Wstringop-overflow" "pr102462" { xfail { vect_slp_v2qi_store_align } } }
  sink (&a);
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member to empty.
struct A1 a1i_0 = { 0, { } };

void ga1i_0_ (void)
{
  a1i_0.a[0] = 0;               // { dg-warning "\\\[-Wstringop-overflow" "" { target { vect_slp_v2qi_store_unalign } } }
  a1i_0.a[1] = 1;               // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a1i_0.a[2] = 2;               // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a = { 0, { } };
  a.a[0] = 0;
  a.a[1] = 1;                   // { dg-warning "\\\[-Wstringop-overflow" }
  a.a[2] = 2;                   // { dg-warning "\\\[-Wstringop-overflow" "pr102462" { xfail { vect_slp_v2qi_store_align } } }
  sink (&a);
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member.
struct A1 a1i_1 = { 0, { 1 } };

void ga1i_1 (void)
{
  a1i_1.a[0] = 0;               // { dg-warning "\\\[-Wstringop-overflow" "" { target { vect_slp_v2qi_store_unalign } } }
  a1i_1.a[1] = 1;               // { dg-warning "\\\[-Wstringop-overflow" "" { xfail { vect_slp_v2qi_store_unalign } } }
  a1i_1.a[2] = 2;               // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a = { 0, { 1 } };   // { dg-warning "\\\[-Wstringop-overflow" "pr102462" { target { vect_slp_v4qi_store_align } } }
  a.a[0] = 1;
  a.a[1] = 2;                   // { dg-warning "\\\[-Wstringop-overflow" "pr102462" { xfail { vect_slp_v4qi_store_align } } }
  a.a[2] = 3;                   // { dg-warning "\\\[-Wstringop-overflow" "pr102462" { xfail { vect_slp_v4qi_store_align } } }
  sink (&a);
}


// Verify no warning for an unknown struct object.
void ga1ip (struct A1i *p)
{
  p->a[0] = 0;
  p->a[3] = 3;                  // { dg-warning "\\\[-Wstringop-overflow" }
  p->a[9] = 9;                  // { dg-warning "\\\[-Wstringop-overflow" }
}


// Verify no warning for an extern struct object.
extern struct A1i a1ix;

void ga1ix (void)
{
  a1ix.a[0] = 0;
  a1ix.a[3] = 3;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a1ix.a[9] = 9;                 // { dg-warning "\\\[-Wstringop-overflow" }
}
