struct foo {
  char a[3];
  char b;
  char c;
};

struct foo bs;
int x;
char y[3];

void bar(void)
{
    memcpy(bs.a, y, 3);
    bs.a[1] = ((x ? &bs.b : &bs.c) - (char *)&bs) - 2;
}
