/* Test that the GP gets properly restored, either by the nonlocal
   receiver or the nested function.  */

#ifndef NO_TRAMPOLINES

typedef __SIZE_TYPE__ size_t;
extern void abort (void);
extern void exit (int);
extern void qsort(void *, size_t, size_t, int (*)(const void *, const void *));

int main ()
{
  __label__ nonlocal;
  int compare (const void *a, const void *b)
  {
    goto nonlocal;
  }

  char array[3];
  qsort (array, 3, 1, compare);
  abort ();

 nonlocal:
  exit (0);
}

#else
int main() { return 0; }
#endif
