
unsigned char a = 6;
int b, c;

static void
fn1 ()
{
  int i = a > 1 ? 1 : a, j = 6 & (c = a && (b = a));
  int d = 0, e = a, f = ~c, g = b || a;
  unsigned char h = ~a;
  if (a)
    f = j;
  if (h && g)
    d = a;
  i = -~(f * d * h) + c && (e || i) ^ f;
  if (i != 1) 
    __builtin_abort (); 
}

int
main ()
{
  fn1 ();
  return 0; 
}
