// { dg-do compile }
// { dg-options "-O2" }

int a, b, c;
void d() {
e : {
  long f;
  long *g = &f;
  if ((a != 0) - (b = 0))
    ;
  else
    a &= (*g %= a *= c) >= (*g || f);
  goto e;
}
}
