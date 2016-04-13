// PR c++/70019 - VLA size overflow not detected
// Compile-time test to verify that attempting to initialize a VLA with
// a string that's longer than the VLA's constant bound is diagnosed at
// compile time.  For a runtime version of the test see vla13.C.

// { dg-do run }
// { dg-additional-options "-Wno-vla" }


void test (int n)
{
  char a1[n][1] = { { "a" } };   // { dg-error "initializer-string for array of chars is too long" }
  (void)a1;

  char a2[1][n] = { { "a" } };
  (void)a2;

  char a3[n][1][1] = { { { "a" } } };   // { dg-error "initializer-string for array of chars is too long" }
  (void)a3;

  char a4[1][1][n] = { { { "a" } } };
  (void)a4;

  char a5[1][n][1] = { { { "a" } } };   // { dg-error "initializer-string for array of chars is too long" }
  (void)a5;

  char a6[n][1][n] = { { { "a" } } };
  (void)a6;


  wchar_t a7[n][1] = { { L"a" } };   // { dg-error "initializer-string for array of chars is too long" }
  (void)a7;

  wchar_t a8[1][n] = { { L"a" } };
  (void)a8;

  wchar_t a9[n][1][1] = { { { L"a" } } };   // { dg-error "initializer-string for array of chars is too long" }
  (void)a9;

  wchar_t a10[1][1][n] = { { { L"a" } } };
  (void)a10;

  wchar_t a11[][n][1] = { { { L"a" } } };   // { dg-error "initializer-string for array of chars is too long" }
  (void)a11;

  wchar_t a12[n][1][n] = { { { L"a" } } };
  (void)a12;
}
