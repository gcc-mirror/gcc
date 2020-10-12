/* { dg-additional-options "-Wno-incompatible-pointer-types" } */
void foo(void)
{
  void (*a[1]) ();
  void (*p) () = a + 1;
}
