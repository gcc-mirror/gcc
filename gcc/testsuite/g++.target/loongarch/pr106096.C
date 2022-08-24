/* PR target/106096
   Reduced from gimple-range-path.cc.  It was miscompiled with -O2 and
   caused ICE (segfault) building stage 2 libgcc.  */

/* { dg-do run } */
/* { dg-options "-O2" } */
enum E
{
  TS_TYPED
} a;
int b, c;
char d, e;

__attribute__ ((cold, noipa, noinline)) void
cold (int *, E, char *, int, char *)
{
  __builtin_trap ();
}

int *
contains_struct_check (E x)
{
  if (a)
    cold (&b, x, &d, c, &e);
  return &b;
}

struct vrange
{
  virtual void set_varying (int *);
};

struct int_range : vrange
{
  int *m_ranges[510];
};

__attribute__ ((noipa, noinline)) void
vrange::set_varying (int *)
{
}

struct Value_Range
{
  Value_Range (int *);
  int_range m_irange;
};

__attribute__ ((noipa, noinline)) Value_Range::Value_Range (int *) {}

struct path_range_query
{
  void ssa_range_in_phi (vrange &);
  bool m_resolve;
};

__attribute__ ((noipa, noinline)) void
path_range_query::ssa_range_in_phi (vrange &r)
{
  if (m_resolve)
    {
      Value_Range (contains_struct_check (TS_TYPED));
      return;
    }
  r.set_varying (contains_struct_check (TS_TYPED));
}

int
main ()
{
  path_range_query prq{ 0 };
  vrange vr;
  prq.ssa_range_in_phi (vr);
  return 0;
}
