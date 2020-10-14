/* PR middle-end/90404 */

const int c1 = 20; /* { dg-message "declared here" } */
int test_1 (void)
{
  *((int*) &c1) = 10; /* { dg-warning "write to 'const' object 'c1'" } */
  return c1;
}

/* Example of writing to a subregion (an element within a const array).  */

const int c2[10]; /* { dg-message "declared here" } */
int test_2 (void)
{
  ((int*) &c2)[5] = 10; /* { dg-warning "write to 'const' object 'c2'" } */
  return c2[5];
}

const char s3[] = "012.45"; /* { dg-message "declared here" } */
int test_3 (void)
{
  char *p = __builtin_strchr (s3, '.');
  *p = 0; /* { dg-warning "write to 'const' object 's3'" } */

  if (__builtin_strlen (p) != 3)
    __builtin_abort ();

  return s3[3] == 0;
}
