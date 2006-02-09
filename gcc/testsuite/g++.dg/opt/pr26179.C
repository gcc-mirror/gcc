/* The problem here is that Load PRE on the tree level
   forgot to handle RETURN_DECL which causes us to ICE. */

// { dg-do compile }
// { dg-options "-O2" }

struct a
{
  int i;
};
void h(struct a&);
void l(void);

struct a g(void)
{
  struct a fl;
  h(fl);
  if (fl.i)
    l();
  fl.i+=2;
  return fl;
}
