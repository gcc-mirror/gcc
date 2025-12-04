/* { dg-do run } */
/* { dg-options "-O2" } */

enum comp_cat_tag
{
  cc_partial_ordering,
  cc_weak_ordering,
  cc_strong_ordering,
  cc_last
};

struct comp_cat_info_t
{
  const char *name;
  const char *members[4];
};

static const struct comp_cat_info_t comp_cat_info[cc_last]
= {
   { "partial_ordering", { "equivalent", "greater", "less", "unordered" } },
   { "weak_ordering", { "equivalent", "greater", "less" } },
   { "strong_ordering", { "equal", "greater", "less" } }
};

volatile const char *gp;

[[gnu::noipa]] static void
check (const char *p)
{
  if (!p)
    __builtin_abort ();
  gp = p;
}

[[gnu::noinline]] int foo (enum comp_cat_tag tag)
{
  for (int i = 0; i < 4; ++i)
    {
      const char *p = comp_cat_info[tag].members[i];
      if (!p)
	continue;;
      check (p);
    }
  return 0;
}

[[gnu::noipa]] enum comp_cat_tag
get_index (void)
{
  return cc_strong_ordering;
}

int main (int argc, char **argv)
{
  foo (get_index ());
  return 0;
}
