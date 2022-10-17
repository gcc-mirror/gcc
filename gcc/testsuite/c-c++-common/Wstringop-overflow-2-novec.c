/* PR middle-end/91458 - inconsistent warning for writing past the end
   of an array member
   { dg-do compile }
   { dg-options "-O2 -fno-tree-vectorize -Wall -Wno-array-bounds -fno-ipa-icf" } */

void sink (void*);

// Exercise trailing one-element array members.

struct A1
{
  char n;
  char a[1];                    // { dg-message "destination object" "note" }
};

// Verify warning for access to a definition with an initializer that doesn't
// initialize the one-element array member.
struct A1 a1__ = { 0 };

void ga1__ (void)
{
  a1__.a[0] = 0;
  a1__.a[1] = 1;                 // { dg-warning "\\\[-Wstringop-overflow" }
  a1__.a[2] = 2;                 // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a = { 1 };
  a.a[0] = 0;
  a.a[1] = 1;                    // { dg-warning "\\\[-Wstringop-overflow" }
  a.a[2] = 2;                    // { dg-warning "\\\[-Wstringop-overflow" }
  sink (&a);
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member to empty.
struct A1 a1_0 = { 0, { } };

void ga1_0_ (void)
{
  a1_0.a[0] = 0;
  a1_0.a[1] = 1;                // { dg-warning "\\\[-Wstringop-overflow" }
  a1_0.a[2] = 2;                // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a = { 1, { } };
  a.a[0] = 0;
  a.a[1] = 1;                   // { dg-warning "\\\[-Wstringop-overflow" }
  a.a[2] = 2;                   // { dg-warning "\\\[-Wstringop-overflow" }
  sink (&a);
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member.
struct A1 a1_1 = { 0, { 1 } };

void ga1_1 (void)
{
  a1_1.a[0] = 0;
  a1_1.a[1] = 1;                // { dg-warning "\\\[-Wstringop-overflow" }
  a1_1.a[2] = 2;                // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a = { 0, { 1 } };
  a.a[0] = 0;
  a.a[1] = 1;                   // { dg-warning "\\\[-Wstringop-overflow" }
  a.a[2] = 2;                   // { dg-warning "\\\[-Wstringop-overflow" }
  sink (&a);
}

// Exercise interior one-element array members (verify they're not
// treated as trailing.

struct A1i
{
  char n;
  char a[1];                    // { dg-message "destination object" }
  char x;
};

// Verify warning for access to a definition with an initializer that doesn't
// initialize the one-element array member.
struct A1i a1i__ = { 0 };

void ga1i__ (void)
{
  a1i__.a[0] = 0;
  a1i__.a[1] = 1;                // { dg-warning "\\\[-Wstringop-overflow" }
  a1i__.a[2] = 2;                // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1i a = { 0 };
  a.a[0] = 0;
  a.a[1] = 1;                    // { dg-warning "\\\[-Wstringop-overflow" }
  a.a[2] = 2;                    // { dg-warning "\\\[-Wstringop-overflow" }
  sink (&a);
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member to empty.
struct A1 a1i_0 = { 0, { } };

void ga1i_0_ (void)
{
  a1i_0.a[0] = 0;
  a1i_0.a[1] = 1;               // { dg-warning "\\\[-Wstringop-overflow" }
  a1i_0.a[2] = 2;               // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a = { 0, { } };
  a.a[0] = 0;
  a.a[1] = 1;                   // { dg-warning "\\\[-Wstringop-overflow" }
  a.a[2] = 2;                   // { dg-warning "\\\[-Wstringop-overflow" }
  sink (&a);
}

// Verify warning for access to a definition with an initializer that
// initializes the one-element array member.
struct A1 a1i_1 = { 0, { 1 } };

void ga1i_1 (void)
{
  a1i_1.a[0] = 0;
  a1i_1.a[1] = 1;               // { dg-warning "\\\[-Wstringop-overflow" }
  a1i_1.a[2] = 2;               // { dg-warning "\\\[-Wstringop-overflow" }

  struct A1 a = { 0, { 1 } };
  a.a[0] = 1;
  a.a[1] = 2;                   // { dg-warning "\\\[-Wstringop-overflow" }
  a.a[2] = 3;                   // { dg-warning "\\\[-Wstringop-overflow" }
  sink (&a);
}
