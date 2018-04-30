/* Test -Wsizeof-pointer-div warnings.  */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

int
f1 (int *array)
{
  int i;
  i = sizeof array / sizeof *array;		/* { dg-warning "does not compute the number of array elements" } */
  i += sizeof array / sizeof array[0];		/* { dg-warning "does not compute the number of array elements" } */
  i += sizeof(array) / sizeof(*array);		/* { dg-warning "does not compute the number of array elements" } */
  i += sizeof(array) / sizeof(array[0]);	/* { dg-warning "does not compute the number of array elements" } */
  i += (sizeof(array)) / (sizeof(array[0]));	/* { dg-warning "does not compute the number of array elements" } */
  i += sizeof(array) / sizeof(int);		/* { dg-warning "does not compute the number of array elements" } */
  i += sizeof(array) / sizeof(char);
  i += sizeof(*array) / sizeof(char);
  i += sizeof(array[0]) / sizeof(char);
  return i;
}

int
f2 (void)
{
  int array[10];
  int i;
  i = sizeof array / sizeof *array;
  i += sizeof array / sizeof array[0];
  i += sizeof(array) / sizeof(*array);
  i += sizeof(array) / sizeof(array[0]);
  i += (sizeof(array)) / (sizeof(array[0]));
  i += sizeof(array) / sizeof(int);
  i += sizeof(array) / sizeof(char);
  i += sizeof(*array) / sizeof(char);
  i += sizeof(array[0]) / sizeof(char);
  return i;
}

int
f3 (int a[])
{
  return sizeof a / sizeof *a;			/* { dg-warning "Wsizeof-array-argument" } */
}
