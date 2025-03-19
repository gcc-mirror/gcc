/* { dg-do compile } */

void jj(void);
int ff1(void) __attribute__((__returns_twice__));
struct s2 {
  int prev;
};
typedef struct s1 {
  unsigned interrupt_flag;
  unsigned interrupt_mask;
  int tag;
  int state;
}s1;
int ff(void);
static inline
int mm(s1 *ec) {
  if (ff())
    if (ec->interrupt_flag & ~(ec)->interrupt_mask)
      return 0;
}
void ll(s1 *ec) {
  int t = 1;
  int state;
  if (t)
  {
    {
      s1 *const _ec = ec;
      struct s2 _tag = {0};
      if (ff1())
	state = ec->state;
      else
	state = 0;
      if (!state)
	mm (ec);
      _ec->tag = _tag.prev;
    }
    if (state)
      __builtin_exit(0);
  }
  jj();
}
