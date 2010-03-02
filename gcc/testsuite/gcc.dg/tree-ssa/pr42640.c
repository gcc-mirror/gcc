/* { dg-do run } */
/* { dg-options "-O2 -ftree-loop-distribution" } */

/* Checks if loop distribution works correctly if the subscript used
   is assigned to a loop invariant value.  */

extern void abort (void);
struct S { int a; int b; int c; };

int get_rr_node_index (int i)
{
  return i;
}

struct S nodes[8];
struct S *rr_node = nodes;
volatile int io_rat = 2;
void
doit (int i, int j)
{
  int s_node, p_node, inode, ipad, iloop;

  for (ipad = 0; ipad < io_rat; ipad++)
    {
      p_node = get_rr_node_index (ipad+2);
      inode = get_rr_node_index (ipad);

      for (iloop = 1; iloop <= 2; iloop++)
	{
	  rr_node[inode].a = i;
	  rr_node[inode].b = j;
	  rr_node[inode].c = ipad;
	  inode = p_node;
	}
    }
}

int
main ()
{
  int i;

  doit (1, 2);

  if (rr_node[0].a != rr_node[1].a
      || rr_node[2].a != rr_node[3].a
      || rr_node[1].a != 1
      || rr_node[0].b != rr_node[1].b
      || rr_node[2].b != rr_node[3].b
      || rr_node[1].b != 2
      || rr_node[0].c != 0
      || rr_node[1].c != 1
      || rr_node[2].c != 0
      || rr_node[3].c != 1)
    abort ();

  return 0;
}
