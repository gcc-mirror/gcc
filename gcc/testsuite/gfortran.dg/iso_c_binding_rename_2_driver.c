void sub2(int **);
void sub3(int **);
void sub4(int **);

int main(int argc, char **argv)
{
  int i = 1; 
  int *ptr;

  ptr = &i;
  sub2(&ptr);
  sub3(&ptr);
  sub4(&ptr);

  return 0;
}
