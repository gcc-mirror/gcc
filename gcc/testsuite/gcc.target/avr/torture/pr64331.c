/* { dg-do run } */

typedef struct
{
  unsigned a, b;
} T2;


__attribute__((__noipa__))
void foo2 (T2 *t, int x)
{
  if (x != t->a)
    {
      t->a = x;
  
      if (x && x == t->b)
	t->a = 20;
    }
}


T2 t;

int main (void)
{
  t.a = 1;
  t.b = 1234;

  foo2 (&t, 1234);

  if (t.a != 20)
    __builtin_abort();

  __builtin_exit (0);

  return 0;
}
