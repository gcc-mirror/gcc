/* { dg-do compile }
 * { dg-options "-std=c23" }
 */

// compatibility of structs in assignment

typedef struct p { int a; } pd_t;

void test1(void)
{
  pd_t y0;
  struct p { int a; } x;
  y0 = x;
}

void test2(void)
{
  struct p { int a; } x;
  struct p y0 = x;
}

void test3(void)
{
  struct p { int a; } x;
  pd_t y0 = x;
}

typedef struct p { int a; } p2_t;

void test4(void)
{
  p2_t x;
  pd_t y0 = x;
}

void test5(void)
{
  struct q { int a; } a;
  struct q { int a; } b;
  a = b;
}


