// PR bootstrap/111601
// { dg-do run { target c++11 } }
// { dg-options "-O2 -fno-exceptions -fno-rtti -fprofile-generate" }
// { dg-require-profiling "-fprofile-generate" }
// { dg-final { cleanup-coverage-files } }

struct tree_base
{
  int code:16;
};
struct saved_scope
{
  void *pad[14];
  int x_processing_template_decl;
};
struct saved_scope *scope_chain;
struct z_candidate
{
  tree_base *fn;
  void *pad[11];
  z_candidate *next;
  int viable;
  int flags;
};

__attribute__((noipa)) struct z_candidate *
splice_viable (struct z_candidate *cands, bool strict_p, bool *any_viable_p)
{
  struct z_candidate *viable;
  struct z_candidate **last_viable;
  struct z_candidate **cand;
  bool found_strictly_viable = false;
  if (scope_chain->x_processing_template_decl)
    strict_p = true;
  viable = (z_candidate *) 0;
  last_viable = &viable;
  *any_viable_p = false;
  cand = &cands;
  while (*cand)
    {
      struct z_candidate *c = *cand;
      if (!strict_p && (c->viable == 1 || ((int) (c->fn)->code) == 273))
	{
	  strict_p = true;
	  if (viable && !found_strictly_viable)
	    {
	      *any_viable_p = false;
	      *last_viable = cands;
	      cands = viable;
	      viable = (z_candidate *) 0;
	      last_viable = &viable;
	    }
	}
      if (strict_p ? c->viable == 1 : c->viable)
	{
	  *last_viable = c;
	  *cand = c->next;
	  c->next = (z_candidate *) 0;
	  last_viable = &c->next;
	  *any_viable_p = true;
	  if (c->viable == 1)
	    found_strictly_viable = true;
	}
      else
	cand = &c->next;
    }
  return viable ? viable : cands;
}

int
main ()
{
  saved_scope s{};
  scope_chain = &s;
  z_candidate z[4] = {};
  z[0].next = &z[1];
  z[1].viable = 1;
  z[1].next = &z[2];
  z[2].viable = 1;
  z[2].next = &z[3];
  bool b;
  z_candidate *c = splice_viable (&z[0], true, &b);
  if (c != &z[1] || z[1].next != &z[2] || z[2].next)
    __builtin_abort ();
  return 0;
}
