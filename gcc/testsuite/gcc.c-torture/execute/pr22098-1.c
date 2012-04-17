extern void abort (void);
extern void exit (int);
typedef __UINTPTR_TYPE__ uintptr_t;
int
main (void)
{
  int a = 0;
  int *p;
  uintptr_t b;
  b = (uintptr_t)(p = &(int []){0, 1, 2}[++a]);
  if (a != 1 || *p != 1 || *(int *)b != 1)
    abort ();
  exit (0);
}
