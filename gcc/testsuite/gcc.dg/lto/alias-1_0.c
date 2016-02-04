/* { dg-lto-do run } */
/* { dg-lto-options { { -O2 -flto } } } */
int val;

__attribute__ ((used))
int *ptr = &val;
__attribute__ ((used))
float *ptr2 = (void *)&val;

extern void typefun(float val);

void link_error (void);

int
main()
{ 
  *ptr=1;
  typefun (0);
  if (*ptr)
    __builtin_abort ();
  return 0;
}

