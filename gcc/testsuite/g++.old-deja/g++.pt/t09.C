// Build don't link: 

struct bs_1 {
  typedef int (*pfi) (void);
};
static bs_1::pfi fp;
