/* PR target/7559
   This testcase was miscompiled on x86-64, because classify_argument
   wrongly computed the offset of nested structure fields.  */

extern void abort (void);

struct A
{
  long x;
};

struct R
{
  struct A a, b;
};

struct R R = { 100, 200 };

void f (struct R r)
{
  if (r.a.x != R.a.x || r.b.x != R.b.x)
    abort ();
}

struct R g (void)
{
  return R;
}

int main (void)
{
  struct R r;
  f(R);
  r = g();
  if (r.a.x != R.a.x || r.b.x != R.b.x)
    abort ();
  return 0;
}
