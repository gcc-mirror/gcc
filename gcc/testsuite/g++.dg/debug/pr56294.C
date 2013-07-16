// { dg-do compile }
// { dg-options "-fno-ipa-sra -fcompare-debug" }

struct comp_cost { int cost; unsigned complexity; };
struct cost_pair { struct iv_cand *cand; };
struct iv_use { unsigned n_map_members; cost_pair *cost_map; };
struct iv_cand { unsigned id; };

unsigned gu;

void
bar (comp_cost, comp_cost)
{
}

void
foo (iv_use *use, iv_cand *cand)
{
  unsigned i, s = cand->id & (use->n_map_members - 1);
  for (i = 0; i < s; i++)
    if (use->cost_map[i].cand)
      goto found;
found:
  use->cost_map[i].cand = cand;
  comp_cost elim_cost, express_cost, bound_cost;
  bar (elim_cost, express_cost);
  gu = express_cost.complexity;
}


