extern int b;
extern void fn1 (int);

void
fn2 (int p)
{
  b = p++;
  fn1 (p);
}
