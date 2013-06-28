/* { dg-do compile { target c } } */
/* { dg-options "-fcilkplus -std=c99 -w" } */

int func (int x)
{
  return x++;
}
int main(void)
{
  int argc = 1;
  __asm volatile ("" : "+r" (argc));
  int array[argc];

  array[:] = 5; /* { dg-error "start-index and length fields necessary for using array notations in variable-length arrays." }  */
  array[0:argc] = 5;               /* This is OK.  */
  array[0:5:2] = 5;                /* This is OK.  */
  array[0:argc:2] = 5;             /* This is OK.  */
  array[0:argc:func (argc-2)] = 5; /* This is OK.  */
  return 0;
}
