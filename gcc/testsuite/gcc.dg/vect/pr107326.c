/* PR107326 */
/* { dg-do compile } */
struct Gsymtab {
  unsigned int : 8;
  unsigned int visited_somewhere : 1;
};

extern struct Gsymtab glob_symtab[];

int
visit_children (int i)
{
  int numvisited = 0;

  while (i < 1)
    {
      if (glob_symtab[i].visited_somewhere)
        ++numvisited;

      ++i;
    }

  return numvisited;
}
