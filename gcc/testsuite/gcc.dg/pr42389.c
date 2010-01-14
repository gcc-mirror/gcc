/* { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling -fsel-sched-pipelining -fsched-pressure" } */

struct s_linked_f_pointer
{
  struct s_linked_f_pointer *next;
  float *fptr;
};
struct s_trace
{
  int index;
};
struct s_rr_cost
{
  float base_cost;
  float acc_cost;
};
extern int num_nets;
extern struct s_trace **trace_head;
extern struct s_rr_cost *rr_cost;
struct s_rr_route
{
  float cost;
};
static int heap_tail;
extern struct s_linked_f_pointer *rr_modified_head;
extern struct s_rr_route *rr_route;

void
empty_heap (void)
{
  heap_tail = 1;
}

void
reset_path_costs (void)
{
  struct s_linked_f_pointer *mod_ptr;
  if (rr_modified_head != ((void *) 0))
    {
      mod_ptr = rr_modified_head;
      while (mod_ptr->next != ((void *) 0))
	{
	  *(mod_ptr->fptr) = 1.e30;
	  mod_ptr = mod_ptr->next;
	}
      rr_modified_head = ((void *) 0);
    }
}

static void
route_net (int inet)
{
  int i;
  for (i = 1; i < inet; i++)
    reset_path_costs ();
  empty_heap ();
  reset_path_costs ();
}

void
pathfinder_update_one_cost (int inet, float pres_fac, float acc_fac)
{
  struct s_trace *tptr;
  int inode = 0;

  tptr = trace_head[inet];
  inode = tptr->index;
  rr_route[inode].cost = rr_cost[inode].base_cost + rr_cost[inode].acc_cost;
}

int
try_route (int n, float x, float y)
{
  int inet, itry;
  float pres_fac;
  for (itry = 1; itry <= n; itry++)
    {
      for (inet = 0; inet < num_nets; inet++)
	{
	  route_net (inet);
	  pathfinder_update_one_cost (inet, pres_fac, x);
	}
      pres_fac *= y;
    }
}

