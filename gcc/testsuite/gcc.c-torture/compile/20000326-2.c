#ifndef NO_LABEL_VALUES
extern int printk(const char *fmt, ...);

void foo (int x, int y)
{
  __label__ here;
  here:
  printk ("", &&here);
}

#else
int x;
#endif
