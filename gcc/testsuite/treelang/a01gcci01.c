/* Driver for treelang test pgm */

int add(int, int);
int subtract(int, int);
int first_nonzero(int, int);

int 
main (int argc, char *argv[])
{
  printf("2:%d\n", add(1,1));
  printf("7:%d\n", add(3,4));
  printf("-1:%d\n", subtract(3,4));
  printf("1:%d\n", subtract(2,1));
  printf("3:%d\n", first_nonzero(0,3));
  printf("0:%d\n", first_nonzero(0,0));
  printf("1:%d\n", first_nonzero(1,0));
  printf("15:%d\n", double_plus_one(7));
  return 0;
}
