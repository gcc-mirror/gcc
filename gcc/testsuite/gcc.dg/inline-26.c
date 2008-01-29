/* PR c/35017 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

static int a = 6;
static const int b = 6;
int c = 6;

inline int
fn1 (void)
{
  return a;		/* { dg-warning "used in inline" } */
}

inline int
fn2 (void)
{
  return b;		/* { dg-warning "used in inline" } */
}

inline int
fn3 (void)
{
  return c;
}

inline int
fn4 (void)
{
  static int d = 6;	/* { dg-warning "declared in inline" } */
  return d;
}

inline int
fn5 (void)
{
  static const int e = 6;
  return e;
}

inline int
fn6 (void)
{
  int f = 6;
  return f;
}

inline int
fn7 (int i)
{
  static const char g[10] = "abcdefghij";
  return g[i];
}

extern inline int
fn8 (void)
{
  return a;
}

extern inline int
fn9 (void)
{
  return b;
}

extern inline int
fn10 (void)
{
  return c;
}

extern inline int
fn11 (void)
{
  static int d = 6;
  return d;
}

extern inline int
fn12 (void)
{
  static const int e = 6;
  return e;
}

extern inline int
fn13 (void)
{
  int f = 6;
  return f;
}

extern inline int
fn14 (int i)
{
  static const char g[10] = "abcdefghij";
  return g[i];
}

static inline int
fn15 (void)
{
  return a;
}

static inline int
fn16 (void)
{
  return b;
}

static inline int
fn17 (void)
{
  return c;
}

static inline int
fn18 (void)
{
  static int d = 6;
  return d;
}

static inline int
fn19 (void)
{
  static const int e = 6;
  return e;
}

static inline int
fn20 (void)
{
  int f = 6;
  return f;
}

static inline int
fn21 (int i)
{
  static const char g[10] = "abcdefghij";
  return g[i];
}
