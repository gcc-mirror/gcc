// PR tree-optimization/97560
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fno-tree-forwprop -fnon-call-exceptions" }

template <typename>
struct pv;

template <typename CY>
struct pv<CY &> {
  typedef CY g7;
};

template <typename Q6>
typename pv<Q6>::g7 hq (Q6 &&lb)
{
  return static_cast<typename pv<Q6>::g7 &&> (lb);
}

struct fk {
  fk *j6;
  fk *od;
};

fk *qi;

struct xz : fk {
  xz (xz &&)
  {
    qi = this;

    if (j6)
      od = this;
  }
};

struct el {
  struct {
    xz ls;
  } eu;
};

struct be : el {
};

be l1 = hq (l1);
