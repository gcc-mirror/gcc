static int ref(void)
{
  union {
    char c[5];
    int i;
  } u;

  __builtin_memset (&u, 0, sizeof(u));
  u.c[0] = 1;
  u.c[1] = 2;
  u.c[2] = 3;
  u.c[3] = 4;

  return u.i;
}

#define MAX(a,b)  (a < b ? b : a)

static int test(void)
{
  char c[MAX(5, sizeof(int))] __attribute__((aligned)) = { 1, 2, 3, 4 };
  return *(int *)c;
}

int main()
{
  int a = test();
  int b = ref();
  if (a != b)
    abort ();
  return 0;
}
