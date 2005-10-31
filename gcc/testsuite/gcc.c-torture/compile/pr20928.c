extern struct bar_t bar;
void *a;
void
foo (void)
{
  void **p = a;
  do {
    *p++ = ((unsigned char *) &bar + ((unsigned long int) 1L << 31));
  } while (p);
}
