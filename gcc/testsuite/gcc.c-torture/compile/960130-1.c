int a[1];

int
main()
{
  extern int a[];
  return *a;
}
