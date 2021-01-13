// PR c++/98642
// { dg-do compile { target c++11 } }

struct base {
  base(void) {}
  base(base &&) = delete;
};

struct foo : public base { };

static foo c1 { };

#if __cpp_aggregate_bases
static foo c2 { {} };
#endif
