void *calloc (__SIZE_TYPE__, __SIZE_TYPE__);

void test_1 (void)
{
  int *p = calloc (0, 1); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'p'" } */
