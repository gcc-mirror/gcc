/* { dg-do compile } */
/* { dg-options "-fcilkplus -std=c99" } */

int func (int x)
{
  return x++;
}
int main(int argc, char **argv)
{
  int array[argc];

  array[:] = 5; /* { dg-error "start-index and length fields necessary for using array notations in variable-length arrays." }  */
  array[0:argc] = 5;               /* This is OK.  */
  array[0:5:2] = 5;                /* This is OK.  */
  array[0:argc:2] = 5;             /* This is OK.  */
  array[0:argc:func (argc-2)] = 5; /* This is OK.  */
  return 0;
}
