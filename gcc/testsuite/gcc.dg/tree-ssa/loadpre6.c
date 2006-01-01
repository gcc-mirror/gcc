/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

union tree_node;
typedef union tree_node *tree;

struct tree_common
{
  tree chain;
};

struct tree_list
{
  struct tree_common common;
  tree value;
};

union tree_node

{
  struct tree_common common;
  struct tree_list list;
};

extern void abort (void) __attribute__((noreturn));

void __attribute__((noinline))
foo (void)
{
  abort ();
}

void __attribute__((noinline))
remove_useless_vars (tree *unexpanded_var_list, int dump_file)
{
  tree var, *cell;
  int c = 0;
  for (cell = unexpanded_var_list; *cell; )
    {
      var = (*cell)->list.value;
      if (var)
        {
          if (dump_file)
            foo ();

          *cell = ((*cell)->common.chain);
          continue;
        }

      cell = &((*cell)->common.chain);
    }
}
extern void *malloc (__SIZE_TYPE__) __attribute__ ((malloc));

int
main (void)
{
  int i;
  tree unexpanded_var_list, last = (tree) 0;

  for (i = 0; i < 2; i++)
    {
      unexpanded_var_list = malloc (sizeof (struct tree_list));
      unexpanded_var_list->list.value = (tree) (long unsigned) (i & 1);
      unexpanded_var_list->common.chain = last;
      last = unexpanded_var_list;
    }

  remove_useless_vars (&unexpanded_var_list, 0);
  return 0;
}
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */

