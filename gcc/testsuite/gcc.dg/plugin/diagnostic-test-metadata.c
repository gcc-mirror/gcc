/* { dg-do compile } */

extern char *gets (char *s);

void test_cwe (void)
{
  char buf[1024];
  gets (buf); /* { dg-warning "never use 'gets' \\\[CWE-242\\\] \\\[STR34-C\\\]" } */
}
