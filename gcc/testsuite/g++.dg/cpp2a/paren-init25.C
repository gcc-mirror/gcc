// PR c++/94149 - make __is_constructible work with paren-init of aggrs.
// { dg-do compile { target c++2a } }

struct nonaggr {
  nonaggr() {}
  int i;
  int j;
};

struct aggr {
  int i;
  int j;
};

static_assert(__is_constructible(aggr, int, int));
static_assert(__is_constructible(aggr, int));
static_assert(!__is_constructible(nonaggr, int, int));

using T = aggr[2];
static_assert(__is_constructible(T, aggr));
static_assert(__is_constructible(T, aggr, aggr));

using N = nonaggr[2];
static_assert(__is_constructible(N, nonaggr));
static_assert(__is_constructible(N, nonaggr, nonaggr));
