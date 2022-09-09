/* { dg-additional-options "-Wunknown-pragmas" } */
void f(void)
{
  #pragma ompx some_vendor_extension  /* { dg-warning "-:ignoring '#pragma ompx some_vendor_extension'" } */
}
