/* Verify that we gracefully handle error functions that don't match
   the signature of GNU's <error.h>.  */

extern void error (void);
extern void error_at_line (void);

void test_1 (void)
{
  error ();
  error_at_line ();
}
