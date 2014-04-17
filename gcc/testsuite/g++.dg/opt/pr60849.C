// { dg-do compile }
// { dg-options "-O2" }

int g;

extern "C" int isnan ();

void foo(float a) {
  int (*xx)(...);
  xx = isnan;
  if (xx(a))
    g++;
}
