/* { dg-do run } */

__attribute__((__noipa__))
void f1(int x, void (*p1 []) (int, int))
{
  int i;
  for (i = 0; i < x; i++)
    p1[i](42, 666);
}

int z1_called = 0;
int w1_called = 0;

__attribute__((__noipa__))
void z1(int a, int b)
{
  if (w1_called || z1_called)
    __builtin_abort();
  z1_called++;
}

__attribute__((__noipa__))
void w1(int a, int b)
{
  if (w1_called || !z1_called)
    __builtin_abort();
  w1_called++;
}

int z2_called = 0;
int w2_called = 0;

__attribute__((__noipa__))
void z2(void)
{
  if (w2_called || z2_called)
    __builtin_abort();
  z2_called++;
}

__attribute__((__noipa__))
void w2(void)
{
  if (w2_called || !z2_called)
    __builtin_abort();
  w2_called++;
}

void (*p2 []) () = { w2, z2 };

__attribute__((__noipa__))
void f2(int x)
{
  void (**q) (void) = p2 + x;
  int i;
  for (i = 0; i < x; i++)
    (*(--q))();
}

__attribute__((__noipa__))
void f3(int x, int (*p3 []) (int))
{
  int i;
  int next = x;
  for (i = 0; i < x; i++)
    next = p3[i](next);
}

int z3_called = 0;
int w3_called = 0;

__attribute__((__noipa__))
int z3(int a)
{
  if (w3_called || z3_called || a != 2)
    __builtin_abort();
  z3_called++;
  return 42;
}

__attribute__((__noipa__))
int w3(int a)
{
  if (w3_called || !z3_called || a != 42)
    __builtin_abort();
  w3_called++;
  return 4096;
}

int (*p4 []) (int) = { z3, w3 };

__attribute__((__noipa__))
void f4(int x)
{
  int (**q) (int) = p4;
  int (**r) (int) = p4 + x;

  int next = x;
  for (; q < r; q++)
    next = (*q)(next);
}

int main(void)
{
  static int (*p3 []) (int) = { z3, w3 };

  static void (*p1 []) (int, int) = { z1, w1 };

  f1(2, p1);
  if (z1_called != 1 || w1_called != 1)
    __builtin_abort();

  f2(2);
  if (z2_called != 1 || w2_called != 1)
    __builtin_abort();

  f3(2, p3);
  if (z3_called != 1 || w3_called != 1)
    __builtin_abort();

  z3_called = 0;
  w3_called = 0;
  f4(2);
  if (z3_called != 1 || w3_called != 1)
    __builtin_abort();

  __builtin_exit(0);
}
