void abort (void);
void exit (int);

struct x {
	int a, b;
} z = { -4028, 4096 };

int foo(struct x *p, int y)
{
  if ((y & 0xff) != y || -p->b >= p->a)
    return 1;
  return 0;
}

int main(void)
{
  if (foo (&z, 10))
    abort ();
  exit (0);
}
