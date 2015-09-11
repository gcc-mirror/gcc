/* { dg-do compile { target ia32 } } */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O2" } */

typedef void *ira_loop_tree_node_t;

extern int end (int);
extern int doo (int);

void
ira_traverse_loop_tree (int bb_p, ira_loop_tree_node_t loop_node,
                        void (*preorder_func) (ira_loop_tree_node_t),
                        void (*postorder_func) (ira_loop_tree_node_t))
{
  int l, r = 0x1, h = 0, j = 0;

  if (preorder_func)
    (*preorder_func) (loop_node);

  if (bb_p)
    {
      for (l = 0; l < end (l); l++)
        {
          r += doo (l);
          h += (l + 1) * 3;
          h %= (l + 1);
          r -= doo (h);
          j += (l + 1) * 7;
          j %= (l + 1);
          r += doo (j);
        }
    }

  if (postorder_func)
    (*postorder_func) (loop_node);
}
/* { dg-final { scan-assembler "jmp\[ \t\]*.%eax" } } */
