/* { dg-do compile } */

struct dm_tree_link {
    int list;
    int node;
};
void fn1(void *p)
{
  0 ? ((struct dm_tree_link *)((char *)p - (char *)&((struct dm_tree_link *)0)->list))->node : 0;
}
