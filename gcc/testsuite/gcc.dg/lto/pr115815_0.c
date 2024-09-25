int a;
volatile int v;
volatile int w;

int __attribute__((destructor))
b() {
  if (v)
    return a + b();
  v = 5;
  return 0;
}

int
main (int argc, char **argv)
{
  w = 1;
  return 0;
}
