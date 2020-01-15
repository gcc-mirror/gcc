#include "analyzer-decls.h"

struct state
{
  int mode;
  int data;
};

extern void do_stuff (struct state *, int);

int test_1 (struct state *s)
{
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
  while (1)
    {
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 exploded nodes" } */
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
      do_stuff (s, s->mode);
    }
}

int test_2 (struct state *s)
{
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
  while (1)
    {
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 exploded nodes" } */
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
      switch (s->mode)
	{
	case 0:
	  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
	  do_stuff (s, 0);
	  break;
	case 1:
	  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
	  do_stuff (s, 17);
	  break;
	case 2:
	  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
	  do_stuff (s, 5);
	  break;
	case 3:
	  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
	  return 42;
	case 4:
	  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 exploded node" } */
	  return -3;
	}
    }
}
