int
test_1 (int *p)
{
  int y = *p;
  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    return 1;
  return 0;
}

int
test_2 (int *p, int *q)
{
  int y = *p;
  *q = 7;
  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    return 1;
  return 0;
}

int
test_3 (int *p, int *q)
{
  *q = *p;
  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    return 1;
  return 0;
}

int
test_4 (int *p, int *q)
{
  *q = 7;
  int y = *p;
  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    return 1;
  return 0;
}

int
test_5 (int *p, int **q)
{
  *q = p;
  if (p) /* 'p' hasn't been dereffed yet, so not warning */
    return 1;
  return 0;
}

int
test_6 (int *p, int *q)
{
  int y = *p;
  int z = *q;
  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    return 1;
  return 0;
}
