/* PR ipa/123227 */

typedef struct Link Link;
struct Link
{
  int val;
  Link *next;
};

int __attribute__((noinline))
get_vals (Link *l)
{
  int v = 0;
  for (; l; l = l->next)
    v |= l->val;
  return v;
}
