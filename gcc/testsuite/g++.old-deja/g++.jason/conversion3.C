void qsort (void *, int, int, int (*)(const void *, const void *));
int f (char *, char *);
void g ()
{
  typedef int (*pf)(void *, void *);
  qsort(0, 0, 0, pf(f));	// ERROR - adding const to function parms
}
