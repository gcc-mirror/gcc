struct foo
{
  int a, b, c;
  int arr[10000000];
};

struct foo s, ss;

main ()
{

  s.b = 2;
  s.c = 3;
  ss.b = 2;
  ss.c = 3;
}
