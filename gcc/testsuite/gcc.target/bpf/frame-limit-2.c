/* { dg-do compile } */
/* { dg-options "-O0 -mframe-limit=256" } */

int
foo ()
{
  long data[32];
  return 0;
}

int
bar ()
{
  long data[33];
  return 0;
} /* { dg-error "stack limit" } */
