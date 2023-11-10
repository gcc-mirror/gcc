/* PR target/71346 */
/* { dg-do compile { target { ! ia32 } }  } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -march=skylake-avx512 -mno-avx512vl" } */

typedef int rvec[3];
int a;
float b, c, d, g;
rvec *e, *f;
void fn2(float h, float g);

void
fn1()
{
  float h;
  for (; a; a++) {
    h += e[a][0] * f[a][0];
    b += e[a][0] * f[a][1];
    c += e[a][2] * f[a][0];
    d += e[a][2] * f[a][1];
    g += e[a][2] * f[a][2];
  }
  fn2(h, g);
}

/* { dg-final { scan-assembler-not "vshufps\[ \\t\]+\[^\n\]*%\xmm(?:1\[6-9\]|\[2-3\]\[0-9\])" } } */
