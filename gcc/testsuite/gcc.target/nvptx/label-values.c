/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic" } */

int
main (void)
{
  goto L2;
 L1:
  return 0;
 L2:
  {
    void *ptr = &&L1; /* { dg-message "sorry, unimplemented: target cannot support label values" "" } */
  }
}
