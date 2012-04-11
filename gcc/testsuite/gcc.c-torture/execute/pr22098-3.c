extern void abort (void);
extern void exit (int);
typedef __UINTPTR_TYPE__ uintptr_t;
int n = 0;
int f (void) { return ++n; }
int
main (void)
{
  int a = 0;
  int *p;
  uintptr_t b;
  b = (uintptr_t)(p = &(int []){0, f(), 2}[1]);
  if (*p != 1 || *(int *)b != 1 || n != 1)
    abort ();
  exit (0);
}
