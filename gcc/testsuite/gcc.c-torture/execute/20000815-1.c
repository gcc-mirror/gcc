struct table_elt
{
  void *exp;
  struct table_elt *next_same_hash;
  struct table_elt *prev_same_hash;
  struct table_elt *next_same_value;
  struct table_elt *prev_same_value;
  struct table_elt *first_same_value;
  struct table_elt *related_value;
  int cost;
  int mode;
  char in_memory;
  char in_struct;
  char is_const;
  char flag;
};

struct write_data
{
  int sp : 1;			 
  int var : 1;			 
  int nonscalar : 1;		 
  int all : 1;			 
};

int cse_rtx_addr_varies_p(void *);
void remove_from_table(struct table_elt *, int);
static struct table_elt *table[32];

void
invalidate_memory (writes)
     struct write_data *writes;
{
  register int i;
  register struct table_elt *p, *next;
  int all = writes->all;
  int nonscalar = writes->nonscalar;

  for (i = 0; i < 31; i++)
    for (p = table[i]; p; p = next)
      {
	next = p->next_same_hash;
	if (p->in_memory
	    && (all
		|| (nonscalar && p->in_struct)
		|| cse_rtx_addr_varies_p (p->exp)))
	  remove_from_table (p, i);
      }
}

int cse_rtx_addr_varies_p(void *x) { return 0; }
void remove_from_table(struct table_elt *x, int y) { abort (); }

int
main()
{
  struct write_data writes;
  struct table_elt elt;

  __builtin_memset(&elt, 0, sizeof(elt));
  elt.in_memory = 1;
  table[0] = &elt;

  __builtin_memset(&writes, 0, sizeof(writes));
  writes.var = 1;
  writes.nonscalar = 1;

  invalidate_memory(&writes);
  return 0;
}
