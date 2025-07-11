/* { dg-do compile } */

extern void here (void);

void test_graphs (void)
{
  here (); /* { dg-error "this is a placeholder error, with graphs" } */
}
