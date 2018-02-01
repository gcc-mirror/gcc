/* Various examples of safe array access for which -Warray-bounds
   shouldn't issue a warning at any optimization level
   (PR tree-optimization/83510).  */

/* { dg-options "-Warray-bounds" } */

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

static unsigned int f_signed(struct xyz * ctx, int number)
{
  switch (number) {
  case 0x9:
    return ctx->a0;
  case 0xA: case 0xB:
  case 0xC: case 0xD: case 0xE: case 0xF:
  case 0x10: case 0x11: case 0x12: case 0x13:
    return arr[number];
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

void test_2_signed (struct xyz * ctx)
{
  int i;

  for (i = 0; i < 10; i++) {
    if (get_flag ())
      wfm(ctx, i, f_signed(ctx, i));
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
