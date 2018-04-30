/* { dg-add-options stack_size } */

#ifdef STACK_SIZE
#define SIZE STACK_SIZE / 8
#else
#define SIZE 10000000
#endif

struct foo
{
  int a, b, c;
  int arr[SIZE];
};

struct foo s, ss;

main ()
{

  s.b = 2;
  s.c = 3;
  ss.b = 2;
  ss.c = 3;
}
