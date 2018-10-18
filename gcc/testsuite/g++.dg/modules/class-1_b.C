// { dg-additional-options "-fmodules-ts" }
module One;

struct Bob::X 
{
  int i;
};


int x = sizeof (Bob::X);

void copy (Bob::Y *dst, Bob::Y const *src)
{
  dst->a = src->a;
  dst->b = src->b;
}
