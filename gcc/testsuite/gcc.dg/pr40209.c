/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-use -fopt-info" } */

void process(const char *s);

struct BaseHolder {
  unsigned int base_;
};

void UInt2Str(struct BaseHolder *b, unsigned int x) {
  static const char digit[] = "0123456789abcdefghijklmnopqrstuvwxyz";
  char buf[100];
  int i = 100;
  do {
    buf[--i] = digit[x % b->base_];
    x /= b->base_;
  } while (x > 0);
  process(buf);
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*\.gcda not found, execution counts estimated.*" } */
