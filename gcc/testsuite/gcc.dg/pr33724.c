/* { dg-do compile } */

/* We ICEd with type-checking enabled.  */

struct xt_entry_target {
  char name[1];
};
struct ipt_entry {
  unsigned char elems[1];
};
void match_different(const unsigned char *);
int dump_entry(struct xt_entry_target *t)
{
  return __builtin_strcmp (t->name, "");
}
void is_same(const struct ipt_entry *a)
{
  match_different(a->elems);
}

