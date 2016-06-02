extern void fn2 (long long);
int a;

void
fn1 ()
{
  long long b[3];
  a = 0;
  for (; a < 3; a++)
    b[a] = 1;
  fn2 (b[1]);
}
