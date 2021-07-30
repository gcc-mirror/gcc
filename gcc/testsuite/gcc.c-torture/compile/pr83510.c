/* Various examples of safe array access for which -Warray-bounds
   shouldn't issue a warning at any optimization level
   (PR tree-optimization/83510).  */

/* { dg-options "-Warray-bounds" } */

/*  This test is XFAILed because thread1 threads a switch statement
    such that the various cases have been split into different
    independent blocks.  One of these blocks exposes an arr[i_27]
    which is later propagated by VRP to be arr[10].  This is an
    invalid access, but the array bounds code doesn't know it is an
    unreachable path.

    However, it is not until dom2 that we "know" that the value of the
    switch index is such that the path to arr[10] is unreachable.  For
    that matter, it is not until dom3 that we remove the unreachable
    path.


    See:
    https://gcc.gnu.org/bugzilla/show_bug.cgi?id=83510
    https://gcc.gnu.org/bugzilla/show_bug.cgi?id=83312

    It's not until here that ranger "knows" that the path is
    unreachable:

    thread1
    vrp1		<-- array bounds checking
    dce2
    stdarg
    cdce
    cselim
    copyprop
    ifcombine
    mergephi3		<-- too late
*/

extern int get_flag (void);

unsigned int arr[10];

struct xyz {
  unsigned int a0;
};

extern void wfm(struct xyz *, int, unsigned int);

static unsigned int f(struct xyz * ctx, unsigned int number)
{
  switch (number) {
  case 0x9:
    return ctx->a0;
  case 0xA: case 0xB:
  case 0xC: case 0xD: case 0xE: case 0xF:
  case 0x10: case 0x11: case 0x12: case 0x13:
    return arr[number - 0xa];
  }
  return 0;
}

int g(struct xyz * ctx) {
  int i;

  for (i = 0; i < 10; i++) {
    wfm(ctx, i, f(ctx, i));
  }

  return 0;
}

int g_signed(struct xyz * ctx) {
  int i;

  for (i = 0; i < 10; i++) {
    wfm(ctx, i, f(ctx, i));
  }

  return 0;
}

void test_2 (struct xyz * ctx)
{
  int i;

  for (i = 0; i < 10; i++) {
    if (get_flag ())
      wfm(ctx, i, f(ctx, i));
  }
}

void test_3 (struct xyz * ctx)
{
  unsigned int i;
  
  for (i = 0; i < 10; i++) {
    switch (i) {
    case 0x9:
      wfm(ctx, i, ctx->a0);
      break;
    case 0xA: case 0xB:
    case 0xC: case 0xD: case 0xE: case 0xF:
    case 0x10: case 0x11: case 0x12: case 0x13:
      if (get_flag ())
	wfm(ctx, i, arr[i - 0xa]);
      break;
    }
  }
}

void test_3_signed (struct xyz * ctx)
{
  int i;
  
  for (i = 0; i < 10; i++) {
    switch (i) {
    case 0x9:
      wfm(ctx, i, ctx->a0);
      break;
    case 0xA: case 0xB:
    case 0xC: case 0xD: case 0xE: case 0xF:
    case 0x10: case 0x11: case 0x12: case 0x13:
      if (get_flag ())
	wfm(ctx, i, arr[i]);
      break;
    }
  }
}

void test_4 (struct xyz * ctx)
{
  unsigned int i, j;
  
  for (i = 0; i < 10; i++) {
    switch (i) {
    case 0x9:
      wfm(ctx, i, ctx->a0);
      break;
    case 0xA: case 0xB:
    case 0xC: case 0xD: case 0xE: case 0xF:
    case 0x10: case 0x11: case 0x12: case 0x13:
      for (j = 0; j < 5; j++)
	wfm(ctx, i, arr[i - 0xa]);
      break;
    }
  }
}
void test_4_signed (struct xyz * ctx)
{
  int i, j;
  
  for (i = 0; i < 10; i++) {
    switch (i) {
    case 0x9:
      wfm(ctx, i, ctx->a0);
      break;
    case 0xA: case 0xB:
    case 0xC: case 0xD: case 0xE: case 0xF:
    case 0x10: case 0x11: case 0x12: case 0x13:
      for (j = 0; j < 5; j++)
	wfm(ctx, i, arr[i]);
      break;
    }
  }
}

void test_5 (struct xyz * ctx)
{
  unsigned int i;
  for (i = 10; i < 20; i++) {
    wfm(ctx, i, arr[i - 10]);
  }    
}

void test_5_signed (struct xyz * ctx)
{
  int i;
  for (i = 10; i < 20; i++) {
    wfm(ctx, i, arr[i - 10]);
  }    
}
