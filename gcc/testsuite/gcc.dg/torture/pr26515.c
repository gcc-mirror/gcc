/* { dg-options "-march=v10" { target cris*-*-* } } */
struct i
{
  long long i_size;
  struct a *i_mapping;
};
struct p
{
  struct a *mapping;
  long index;
};
extern void b (struct p*, unsigned);
extern void u (struct p*);
void
block_page_mkwrite (struct i *i, struct p *p)
{
  unsigned end = 0;
  long long size = 0;
  size = i->i_size;
  if ((p->mapping != i->i_mapping))
    goto out_unlock;
  if (((p->index + 1) << 13) > size)
    end = size & ~(~(((1UL) << 13) - 1));
  b (p, end);
out_unlock:
  u (p);
}
