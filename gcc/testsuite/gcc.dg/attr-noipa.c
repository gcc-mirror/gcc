/* Test the noipa attribute.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

static inline int __attribute__((noipa))
fn1 (void) /* { dg-warning "inline function \[^\n\]* given attribute 'noinline'" "" } */
{
  return 1;
}

/* Verify the function is not inlined into its caller.  */

static __attribute__((noipa)) int
fn2 (int x, int y)
{
  return x + y;
}

int
fn3 (int x)
{
  return fn2 (x, 0);
}

/* { dg-final { scan-tree-dump "= fn2 \\(" "optimized" } } */

void fn4 (char *);

/* Verify the function is not cloned.  */

__attribute__((__noipa__)) static int
fn5 (int x, int y)
{
  char *p = __builtin_alloca (x + y);
  fn4 (p);
  return x + y;
}

int
fn6 (int x)
{
  return fn5 (x, 2);
}

/* { dg-final { scan-tree-dump "= fn5 \\(" "optimized" } } */
/* { dg-final { scan-tree-dump-not "fn5\\.constprop" "optimized" } } */

/* Verify we still remove unused function calls, even if they have
   noipa attribute.  */

static void fn7 (void) __attribute__((noipa));
static void
fn7 (void)
{
}

/* { dg-final { scan-tree-dump-not "fn7 \\(" "optimized" } } */

/* Verify noipa functions are not ICF optimized.  */

static __attribute__((noipa)) int
fn8 (int x)
{
  return x + 12;
}

static __attribute__((noipa)) int
fn9 (int x)
{
  return x + 12;
}

int
fn10 (int x)
{
  return fn8 (x) + fn9 (x);
}

/* { dg-final { scan-tree-dump "fn8 \\(int" "optimized" } } */
/* { dg-final { scan-tree-dump "fn9 \\(int" "optimized" } } */

/* Verify IPA-VRP is not performed.  */

void fn11 (void);

static int __attribute__((noipa))
fn12 (int x)
{
  if (x < 6 || x >= 29)
    fn11 ();
}

void
fn13 (int x)
{
  fn12 (6 + (x & 15));
}

/* { dg-final { scan-tree-dump "fn11 \\(\\)" "optimized" } } */

void fn14 (void);

__attribute__((noipa)) static int
fn15 (int x)
{
  return x & 7;
}

int
fn16 (int x)
{
  x = fn15 (x);
  if (x < 0 || x >= 7)
    fn14 ();
}

/* { dg-final { scan-tree-dump "fn14 \\(\\)" "optimized" } } */

/* Verify IPA BIT CP is not performed.  */

void fn17 (void);

__attribute__((noipa)) static int
fn18 (int x)
{
  if (x & 8)
    fn17 ();
}

void
fn19 (void)
{
  fn18 (1);
  fn18 (2);
  fn18 (4);
  fn18 (16);
  fn18 (32);
  fn18 (64);
}

/* { dg-final { scan-tree-dump "fn17 \\(\\)" "optimized" } } */

/* Ensure pure/const discovery is not performed.  */

int var1;
void fn20 (void);

__attribute__((noipa)) static int
fn21 (int x, int y)
{
  return x * y;
}

int
fn22 (void)
{
  var1 = 7;
  asm volatile ("" : "+g" (var1) : : "memory");
  int a = var1;
  int b = fn21 (a, a);
  if (a != var1)
    fn20 ();
  return b;
}

/* { dg-final { scan-tree-dump "fn20 \\(\\)" "optimized" } } */

/* Verify IPA alignment propagation is not performed.  */

static __attribute__ ((aligned(16))) char var2[32];
void fn23 (void);

__attribute__((noipa)) static void
fn24 (char *p)
{
  if ((((__UINTPTR_TYPE__) p) & 15) != 0)
    fn23 ();
  asm ("");
}

void
fn25 (void)
{
  fn24 (var2);
  fn24 (var2 + 16);
}

/* { dg-final { scan-tree-dump "fn20 \\(\\)" "optimized" } } */
