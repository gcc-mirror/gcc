/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

typedef union tree_node *tree;

struct tree_common
{
  tree chain;
};

struct tree_decl
{
  struct tree_common common;
  tree name;
};


union tree_node
{
  struct tree_common common;
  struct tree_decl decl;
};

int pedantic;

void
finish_struct (tree t, tree fieldlist, tree attributes)
{
  union tree_node * x;

  if (pedantic)
    {
      x = fieldlist;
      if (x->decl.name == 0)
	{
	  while (x)
	    x = x->common.chain;
	  foo (fieldlist);
	}
    }

  x = fieldlist;
  if (x)
    {
      do
	{
	  x = x->common.chain;
	} while (x != 0);
    }
  
  bar1 (&fieldlist);
}
