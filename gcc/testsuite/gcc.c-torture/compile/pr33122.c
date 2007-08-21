struct dis386 {
  const char *x;
};

static const struct dis386 float_reg[][2] = {
  { { "fadd" }, { "fadd" } },
};

void foo(int i, int j)
{
  const struct dis386 *dp;

  dp = &float_reg[i - 1][j];
}
