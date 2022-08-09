/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-threadfull1-stats" } */

struct S;
struct S { struct S *next; };
int foo (struct S *chain, _Bool is_ctor, _Bool is_dtor)
{
  int num_args = 0;
  if (chain) /* A */
    {
      do {
          num_args++;
          chain = chain->next;
          if (!chain)
            break;
      } while (1);
    }
  if (is_ctor)
    num_args++; /* B */
  if (is_dtor)
    num_args++;
  else
    {
      if (num_args > 2) /* C */
        __builtin_puts ("x");
    }
  return num_args;
}

/* We want to thread both paths from A with NULL chain to C, the one through
   B and one around it.
   ???  Ideally we'd thread one "path" containing the half-diamond with B.  */
/* { dg-final { scan-tree-dump "Jumps threaded: 2" "threadfull1" } } */
