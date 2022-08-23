// TODO: remove need for the taint option:
/* { dg-additional-options "-fanalyzer-checker=taint" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

struct raw_ep {
  /* ...snip... */
  int state;
  /* ...snip... */
};

struct raw_dev {
  /* ...snip... */
  struct raw_ep eps[30];
  int eps_num;
  /* ...snip... */
};

int   __attribute__((tainted_args))
simplified_raw_ioctl_ep_disable(struct raw_dev *dev, unsigned long value)
{
  int ret = 0, i = value;

  if (i < 0 || i >= dev->eps_num) {
    ret = -16;
    goto out_unlock;
  }
  if (dev->eps[i].state == 0) { /* { dg-bogus "attacker-controlled" } */
    ret = -22;
    goto out_unlock;
  }

out_unlock:
  return ret;
}

int   __attribute__((tainted_args))
test_2(struct raw_dev *dev, int i)
{
  int ret = 0;

  if (i < 0 || i >= dev->eps_num) {
    ret = -16;
    goto out_unlock;
  }
  if (dev->eps[i].state == 0) { /* { dg-bogus "attacker-controlled" } */
    ret = -22;
    goto out_unlock;
  }

out_unlock:
  return ret;
}
