/* { dg-require-effective-target label_values } */

extern int printk(const char *fmt, ...);

void foo (int x, int y)
{
  __label__ here;
  here:
  printk ("", &&here);
}
