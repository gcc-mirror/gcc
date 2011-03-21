/* { dg-do run } */
/* { dg-options "-fipa-pta" } */
/* { dg-additional-sources "pr47426-2.c" } */

void bar (int *i);

static void
foo (int *i)
{
  if (*i)
    bar (i);
  if (*i)
    __builtin_abort();
}

typedef void tfoo (int *);

tfoo *
getfoo (void)
{
  return &foo;
}

