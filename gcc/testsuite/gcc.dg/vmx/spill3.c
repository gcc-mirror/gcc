#include <altivec.h>
extern void g(vector unsigned char, ...);
extern vector unsigned char v(void);
extern double d(void);
extern int i(void);

static vector unsigned char v1l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v2l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v3l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v4l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v5l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v6l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v7l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v8l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v9l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v10l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v11l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static vector unsigned char v12l = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static double d1l = 0;
static double d2l = 0;
static double d3l = 0;
static double d4l = 0;
static double d5l = 0;
static double d6l = 0;
static double d7l = 0;
static double d8l = 0;
static double d9l = 0;
static double d10l = 0;
static double d11l = 0;
static double d12l = 0;
static double d13l = 0;
static double d14l = 0;
static double d15l = 0;
static double d16l = 0;
static double d17l = 0;
static double d18l = 0;
static int i1l = 0;
static int i2l = 0;
static int i3l = 0;
static int i4l = 0;
static int i5l = 0;
static int i6l = 0;
static int i7l = 0;
static int i8l = 0;
static int i9l = 0;
static int i10l = 0;
static int i11l = 0;
static int i12l = 0;
static int i13l = 0;
static int i14l = 0;
static int i15l = 0;
static int i16l = 0;
static int i17l = 0;
static int i18l = 0;
static int i19l = 0;

void f()
{
  char buffer[23];
  vector unsigned char v1l = v();
  vector unsigned char v2l = v();
  vector unsigned char v3l = v();
  vector unsigned char v4l = v();
  vector unsigned char v5l = v();
  vector unsigned char v6l = v();
  vector unsigned char v7l = v();
  vector unsigned char v8l = v();
  vector unsigned char v9l = v();
  vector unsigned char v10l = v();
  vector unsigned char v11l = v();
  vector unsigned char v12l = v();

  double d1l = d();
  double d2l = d();
  double d3l = d();
  double d4l = d();
  double d5l = d();
  double d6l = d();
  double d7l = d();
  double d8l = d();
  double d9l = d();
  double d10l = d();
  double d11l = d();
  double d12l = d();
  double d13l = d();
  double d14l = d();
  double d15l = d();
  double d16l = d();
  double d17l = d();
  double d18l = d();

  int i1l = i();
  int i2l = i();
  int i3l = i();
  int i4l = i();
  int i5l = i();
  int i6l = i();
  int i7l = i();
  int i8l = i();
  int i9l = i();
  int i10l = i();
  int i11l = i();
  int i12l = i();
  int i13l = i();
  int i14l = i();
  int i15l = i();
  int i16l = i();
  int i17l = i();
  int i18l = i();
  int i19l = i();

  if (d1l)
    g(v1l, v2l, v3l, v4l, v5l, v6l, v7l, v8l, v9l, v10l, v11l, v12l,
      d1l, d2l, d3l, d4l, d5l, d6l, d7l, d8l, d9l, d10l, d11l, d12l,
      d13l, d14l, d15l, d16l, d17l, d18l,
      i1l, i2l, i3l, i4l, i5l, i6l, i7l, i8l, i9l, i10l, i11l, i12l,
      i13l, i14l, i15l, i16l, i17l, i18l, i19l);

  g(v1l, buffer,
    d1l, d2l, d3l, d4l, d5l, d6l, d7l, d8l, d9l, d10l, d11l, d12l,
    d13l, d14l, d15l, d16l, d17l, d18l,
    i1l, i2l, i3l, i4l, i5l, i6l, i7l, i8l, i9l, i10l, i11l, i12l,
    i13l, i14l, i15l, i16l, i17l, i18l, i19l);
}

double
d(void)
{
  static double zero;
  return zero;
}

int
i(void)
{
  static int non_zero;
  return ++non_zero;
}

vector unsigned char
v(void)
{
  static vector unsigned char zero;
  return zero;
}

void
g(vector unsigned char a, ...)
{
}

int main() 
{
  f();
  return 0;
}
