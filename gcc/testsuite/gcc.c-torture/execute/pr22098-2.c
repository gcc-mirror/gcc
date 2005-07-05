extern void abort (void);
extern void exit (int);
typedef __SIZE_TYPE__ size_t;
int
main (void)
{
  int a = 0;
  int *p;
  size_t b;
  b = (size_t)(p = &(int []){0, 1, 2}[1]);
  if (*p != 1 || *(int *)b != 1)
    abort ();
  exit (0);
}
