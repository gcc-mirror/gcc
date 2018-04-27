// { dg-require-effective-target c++14 }

void test_1 ()
{
  auto lambda = [val = 2](){};
  lambda.val; // { dg-bogus "did you mean" }
  // { dg-error "has no member named 'val'" "" { target *-*-* } .-1 }
}

int test_2 ()
{
  auto lambda = [val = 2](){ return val; };

  // TODO: should we issue an error for the following assignment?
  lambda.__val = 4;

  return lambda();
}
