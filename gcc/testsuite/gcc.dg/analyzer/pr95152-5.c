/* { dg-additional-options "-Wno-incompatible-pointer-types" } */
/* { C only: Wno-incompatible-pointer-types' is not valid for C++. */
void foo(void)
{
  void (*a[1]) ();
  void (*p) () = a + 1;
}
