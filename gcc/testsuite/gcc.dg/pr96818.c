// { dg-do compile }
// { dg-options "-O2" }

int a, b, c;
void d() {
  unsigned short e;
  while (b)
    ;
  e = (e + 5) / a;
  switch (e)
  case 0:
  case 3:
    c = a;
}
