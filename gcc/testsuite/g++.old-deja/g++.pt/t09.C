// { dg-do assemble  }

struct bs_1 {
  typedef int (*pfi) (void);
};
static bs_1::pfi fp;
