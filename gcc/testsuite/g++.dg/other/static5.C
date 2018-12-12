struct S
{
  static int i;
  const static double d;
};

static int S::i;  // { dg-error "1:.static. may not be used" }
const static double S::d = 1.0;  // { dg-error "7:.static. may not be used" }
