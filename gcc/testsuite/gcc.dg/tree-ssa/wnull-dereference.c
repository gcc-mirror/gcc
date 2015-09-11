/* { dg-do compile } */ 
/* PR c/16531 */
/* { dg-options "-O2 -fdelete-null-pointer-checks -Wnull-dereference" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

#ifndef __cplusplus
#define NULL (void *)0
#else
#define NULL nullptr
#endif

struct t
{
  int bar;
};

struct t2
{
  struct t *s;
};

void test1 ()
{
  struct t *s = NULL;
  s->bar = 1;  /* { dg-warning "null" } */
}

void test2 (struct t *s)
{
  if (s == NULL && s->bar > 2)  /* { dg-warning "null" } */
    return;

  s->bar = 3;
}

void test3 (struct t *s)
{
  if (s != NULL || s->bar > 2)  /* { dg-warning "null" } */
    return;

  s->bar = 3;  /* { dg-warning "null" } */
}

int test4 (struct t *s)
{
  if (s != NULL && s->bar > 2)  /* { dg-bogus "null" } */
    return 1;
  return 0;
}

int test5 (struct t *s)
{
  if (s == NULL || s->bar > 2)  /* { dg-bogus "null" } */
    return 1;
  return 0;
}

int test6 (struct t2 *s)
{
  if (s->s == 0 && s->s->bar == 0)  /* { dg-warning "null" } */
    return 1;
  return 0;
}

int test7 (struct t *s)
{
  s = 0;
  return s->bar;  /* { dg-warning "null" } */
}

int test8 ()
{
  return ((struct t *)0)->bar;  /* { dg-warning "null" } */
}

void test9 (struct t **s)
{
  if (s == 0)
    *s = 0;  /* { dg-warning "null" } */
}


