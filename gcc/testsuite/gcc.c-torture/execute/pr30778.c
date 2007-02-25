extern void *memset (void *, int, unsigned long);
extern void abort (void);

struct reg_stat {
  void *last_death;
  void *last_set;
  void *last_set_value;
  int   last_set_label;
  char  last_set_sign_bit_copies;
  int   last_set_mode : 8;
  char  last_set_invalid;
  char sign_bit_copies;
  long nonzero_bits;
};

static struct reg_stat *reg_stat;

void __attribute__((noinline))
init_reg_last (void)
{
  memset (reg_stat, 0, __builtin_offsetof (struct reg_stat, sign_bit_copies));
}

int main (void)
{
  struct reg_stat r;

  reg_stat = &r;
  r.nonzero_bits = -1;
  init_reg_last ();
  if (r.nonzero_bits != -1)
    abort ();
  return 0;
}
