// prms-id: 789

extern "C" void printf (char *, ...);
struct foo
{
  static int count;
  virtual void print (int i, int j) { printf ("foo[%d][%d] = %d\n", i, j, x); }
  int x;
  foo () { x = count++; }
};
int foo::count;
struct bar : virtual public foo
{
  virtual void print (int i, int j) { printf ("bar[%d][%d] = %d\n", i, j, x); }
};

// bar array[3][3];
foo array[3][3];

int main ()
{
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 3; j++) {
//      printf("&a[%d][%d] = %x\n", i, j, (void *)&array[i][j]);
      array[i][j].print (i, j);
    }
  return 0;
}
