/* { dg-do compile } */
/* { dg-additional-options "-fno-report-bug" } */

extern void inject_ice (void);

void test_1 (void)
{
  inject_ice (); /* { dg-ice "I'm sorry Dave, I'm afraid I can't do that" } */
  /* { dg-regexp "during GIMPLE pass: crash_test" } */
}
