static int Sub(int a, int b) {
  return  b -a;
}

static unsigned Select(unsigned a, unsigned b, unsigned c) {
  const int pa_minus_pb =
      Sub((a >>  8) & 0xff, (b >>  8) & 0xff) + 
      Sub((a >>  0) & 0xff, (b >>  0) & 0xff); 
  return (pa_minus_pb <= 0) ? a : b;
}

__attribute__((noinline)) unsigned Predictor(unsigned left, const unsigned* const top) {
  const unsigned pred = Select(top[1], left, top[0]);
  return pred;
}

int main(void) {
  const unsigned top[2] = {0xff7a7a7a, 0xff7a7a7a};
  const unsigned left = 0xff7b7b7b;
  const unsigned pred = Predictor(left, top /*+ 1*/);
  if (pred == left)
    return 0;
  return 1;
}



