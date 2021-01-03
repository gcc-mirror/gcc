/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */

struct bitmapped_struct {
    unsigned one : 1;
    unsigned two : 1;
    unsigned three : 1;
    unsigned four : 1;
    unsigned five : 1;
    unsigned six : 1;
    unsigned seven : 1;
    unsigned eight : 1;
};

/* Check that hwasan allows valid bitfield accesses. */
int __attribute__ ((noinline))
handle_unaligned_access (struct bitmapped_struct *foo)
{
  if (foo->three)
    return foo->four;

  foo->five = 1;
  return 1;
}

int main()
{
  struct bitmapped_struct myvar = {0};
  handle_unaligned_access (&myvar);
  return 0;
}
