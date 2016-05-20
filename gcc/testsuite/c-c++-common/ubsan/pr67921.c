/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

struct s
{
  int n;
  int arr[][6];
};
void bar (int);
void foo (struct s *ptr)
{
  int i;
  for (; i < 2; i++)
    for (; ptr->n;)
      {
	int *a = ptr->arr[i];
	int b[66];
	int j = 0;

	for (; j < 56; j++)
	  bar (a[j] - b[j]);
    }
}
