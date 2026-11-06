/* { dg-do run } */

struct Q {
  float x;
  int y;
};

int sum;

int set(void *p, long long v, int n)
{
  int r = 0;
  for (int i = 0; i < n; i++)
    switch (i % 8) {
    case 0: r = r * 3 + 1; break;
    case 1: r = r * 5 + 2; break;
    case 2: r = r * 7 + 3; break;
    case 3: r = r * 9 + 4; break;
    case 4: r = r * 11 + 5; break;
    case 5: r = r * 13 + 6; break;
    case 6: r = r * 15 + 7; break;
    case 7: r = r * 17 + 8; break;
    }
  *(long long *)p = v;
  return r;
}

int get(struct Q *q, int n)
{
  int r = 0;
  for (int i = 0; i < n; i++)
    switch (i % 8) {
    case 0: r = r * 3 + 1; break;
    case 1: r = r * 5 + 2; break;
    case 2: r = r * 7 + 3; break;
    case 3: r = r * 9 + 4; break;
    case 4: r = r * 11 + 5; break;
    case 5: r = r * 13 + 6; break;
    case 6: r = r * 15 + 7; break;
    case 7: r = r * 17 + 8; break;
    }
  return r + q->x + q->y;
}

volatile int n_;

int main(int argc, char **argv)
{
  int n = n_;
  char *b = __builtin_malloc(32);
  for (int k = 1; k < 4; k++) {
    struct Q *q = (struct Q *)(b + k * 8);
    q->x = 1;
    q->y = 2;
    sum += q->y;
  }
  sum += set(b + 16, 5, n);
  for (int k = 0; k < 8; k++)
    b[8 + k] = b[24 + k];
  sum += get((struct Q *)(b + 8), n);
  if (sum != 9 || *(long long *)(b + 16) != 5)
    __builtin_abort ();
  return 0;
}
