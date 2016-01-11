/* { dg-do compile } */
/* { dg-options "-O3 -g" } */

/* This was reduced from gcc/tree-vect-slp.c by H.J.Lu.  */

struct xxx_def;
typedef xxx_def *xxx;

union rtxxx
{
  const char *rt_str;
  xxx rt_xxx;
};

struct xxx_def {
  union u {
    rtxxx fld[1];
  } u;
};

extern xxx bar (void);
extern int foo1 (xxx);

static inline xxx
foo2 (xxx arg0, xxx arg1)
{
  xxx rt;
  rt = bar ();
  (((rt)->u.fld[0]).rt_xxx) = arg0;
  (((rt)->u.fld[1]).rt_xxx) = arg1;
  return rt;
}

static inline xxx
foo4 (const char *arg0 )
{
  xxx rt;
  rt = bar ();
  (((rt)->u.fld[0]).rt_str) = arg0;
  (((rt)->u.fld[1]).rt_xxx) = (xxx) 0;
  return rt;
}

extern xxx foo5 (long);

struct address_cost_data
{
  unsigned costs[2][2][2][2];
};

void
get_address_cost (address_cost_data *data)
{
  unsigned acost;
  long i;
  long rat, off = 0;
  unsigned sym_p, var_p, off_p, rat_p;
  xxx addr, base;
  xxx reg0, reg1;

  reg1 = bar ();
  addr = foo2 (reg1, (xxx) 0);
  rat = 1;
  acost = 0;
  reg0 = bar ();
  reg1 = bar ();

  for (i = 0; i < 16; i++)
    {
      sym_p = i & 1;
      var_p = (i >> 1) & 1;
      off_p = (i >> 2) & 1;
      rat_p = (i >> 3) & 1;

      addr = reg0;
      if (rat_p)
	addr = foo2 (addr, foo5 (rat)) ;

      if (var_p)
	addr = foo2 (addr, reg1);

      if (sym_p)
	base = foo4 ("");
      else if (off_p)
	base = foo5 (off);
      else
	base = (xxx) 0;

      if (base)
	addr = foo2 (addr, base);

      acost = foo1 (addr);
      data->costs[sym_p][var_p][off_p][rat_p] = acost;
    }
}
