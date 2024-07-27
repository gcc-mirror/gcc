/* Generate a warning with a diagnostic_path within a header.  */

void test (void *ptr)
{
  __builtin_free (ptr); // 1st
  __builtin_free (ptr); // 2nd
}
