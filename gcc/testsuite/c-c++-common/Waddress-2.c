/* PR c/47931 - missing -Waddress warning for comparison with NULL
  { dg-do compile }
  { dg-options "-Waddress" } */

#define NULL ((void *) 0)

int i;

int f0 (void)
{
  return &i != 0;   /* { dg-warning "the address of .i. will never be NULL" } */
}

int f1 (void)
{
  return &i != (void *) 0;   /* { dg-warning "the address of .i. will never be NULL" } */
}

int f2 (void)
{
  return &i != NULL;   /* { dg-warning "the address of .i. will never be NULL" } */
}
