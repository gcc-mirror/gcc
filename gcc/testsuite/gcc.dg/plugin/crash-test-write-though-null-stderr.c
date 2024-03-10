/* { dg-do compile } */
/* { dg-additional-options "-fno-report-bug" } */

extern void inject_write_through_null (void);

void test_inject_write_through_null (void)
{
  inject_write_through_null (); /* { dg-ice "Segmentation fault" } */ 
  /* { dg-regexp "during GIMPLE pass: crash_test" } */
}
