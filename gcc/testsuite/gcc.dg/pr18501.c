/* Expected uninitialized variable warning.  */

/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

unsigned bmp_iter_set ();
int something (void);

void
bitmap_print_value_set (void)
{
  unsigned first;	/* { dg-warning "may be used" "conditional in loop" } */
  
  for (; bmp_iter_set (); )
    {
      if (!first)
	something ();
      first = 0;
    }
}
