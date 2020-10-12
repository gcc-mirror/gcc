/* { dg-additional-options "-O1" } */

int oh[1];
int *x3;

int *
cm (char *m0)
{
  return oh;
}

void
ek (void)
{
  for (;;)
    {
      char *b2 = 0;

      if (*b2 != 0) /* { dg-warning "dereference of NULL" } */
	++b2;

      x3 = cm (b2);
    }
}
