/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */

int *a;
int b, c, d, e;
void f() {
  int g;
  for (;;)
    for (; b;)
      if (d)
        for (; c;)
          if (g)
            e += a[1] = a[2] = e;
}
