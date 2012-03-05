// { dg-lto-do run }

extern "C" {
  static int f4(int);

int f5(int a) {
  extern int f4(int);
  return f4(a);
}
}

int f4(int a) { return 4+a; }

int main(int argc, char *argv[])
{
  int a = f4(1);
  return !(a == 5);
}
