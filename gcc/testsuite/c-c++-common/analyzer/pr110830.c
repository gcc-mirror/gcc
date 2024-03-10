typedef __SIZE_TYPE__ size_t;

void free(void *);
void *malloc(__SIZE_TYPE__);

extern int ext();

void test_supersedes ()
{
  int *p = (int *)malloc(sizeof(int));
  free(p);
  int x = *p + 4; /* { dg-warning "use after 'free' of 'p'" } */
  /* { dg-bogus "use of uninitialized value '\\*p" "" { target *-*-* } .-1 } */
}

int *called_by_test0()
{
  int *p = 0;
  if (ext())
  {
    p = (int *)malloc(sizeof(int));
    free(p);
    return p;
  }
  else
    return (int *)malloc(sizeof(int));
}

void test0()
{
  int *y = called_by_test0();
  int x = 0;
  if (y != 0)
    x = *y; /* { dg-warning "use after 'free' of 'y'" } */
    /* { dg-warning "use of uninitialized value '\\*y'" "don't supersede warnings with incompatible cfg path" { target *-*-* } .-1 } */
  free(y); /* { dg-warning "double-'free'" }  */
}

void test1()
{
  int *p = 0;
  if (ext())
  {
    p = (int *)malloc(sizeof(int));
    free(p);
  }
  else
    p = (int *)malloc(sizeof(int));

  int x = 0;
  if (p != 0)
    x = *p; /* { dg-warning "use after 'free' of 'p'" } */
    /* { dg-warning "use of uninitialized value '\\*p'" "don't supersede warnings with incompatible cfg path" { target *-*-* } .-1 } */
  free(p); /* { dg-warning "double-'free'" }  */
}

void test2()
{
  int *p = 0;
  p = (int *)malloc(sizeof(int));
  if (ext())
    free(p);

  int x = 0;
  if (p != 0)
    x = *p; /* { dg-warning "use after 'free' of 'p'" } */
    /* { dg-warning "use of uninitialized value '\\*p'" "don't supersede warnings with incompatible cfg path" { target *-*-* } .-1 } */
  free(p); /* { dg-warning "double-'free'" }  */
}

void test3()
{
  int *p = 0;
  p = (int *)malloc(sizeof(int));
  int i = 100;
  while (i--)
  {
    int x = 0;
    if (p != 0)
      x = *p; /* { dg-warning "use after 'free' of 'p'" } */
      /* { dg-warning "use of uninitialized value '\\*p'" "don't supersede warnings with incompatible cfg path" { target *-*-* } .-1 } */
    p = (int *)malloc(sizeof(int));
    free(p);
  }

  free(p); /* { dg-warning "double-'free'" }  */
}


void test4()
{
  int *p = 0;
  if (ext())
  {
    p = (int *) malloc(sizeof(int));
    if (ext () > 5)
    {
      mal:
      free (p);
    }
  }
  else {
    goto mal;
  }

  int x = 0;
  if (p != 0)
    x = *p; /* { dg-warning "use after 'free' of 'p'" } */
    /* { dg-warning "use of uninitialized value '\\*p'" "" { target *-*-* } .-1 } */
  free(p); /* { dg-warning "double-'free'" }  */
}
