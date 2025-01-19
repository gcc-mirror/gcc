/* { dg-do run } */

/* Test that we do NOT perform unsound transformations for any of these cases.
   Forwarding blocks to the exit block used to enable some of them.  */

[[gnu::noinline]]
int f0 (int a, int b) {
  if ((a & 1))
    return 0;
  if (b)
    return 1;
  if (!(a & 2))
    return 0;
  else
    return 1;
}

[[gnu::noinline]]
int f1 (int a, int b) {
  if (!(a & 1))
    return 0;
  if (b)
    return 1;
  if ((a & 2))
    return 1;
  else
    return 0;
}

[[gnu::noinline]]
int f2 (int a, int b) {
  if ((a & 1))
    return 0;
  if (b)
    return 1;
  if (!(a & 2))
    return 0;
  else
    return 1;
}

[[gnu::noinline]]
int f3 (int a, int b) {
  if (!(a & 1))
    return 0;
  if (b)
    return 1;
  if ((a & 2))
    return 1;
  else
    return 0;
}

int main() {
  if (f0 (0, 1) != 1)
    __builtin_abort();
  if (f1 (1, 1) != 1)
    __builtin_abort();
  if (f2 (2, 1) != 1)
    __builtin_abort();
  if (f3 (3, 1) != 1)
    __builtin_abort();
}
