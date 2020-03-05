/* PR c/90677 */
/* { dg-do compile } */
/* { dg-options "-W -Wall" } */

struct cgraph_node;
union tree_node;
typedef union tree_node *tree;
union gimple_statement_d;
typedef union gimple_statement_d *gimple;
struct cgraph_node *cgraph_node (tree);
void foo (int, const char *, ...) __attribute__((__format__(__gcc_diag__, 2, 3)));
