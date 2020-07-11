// PR c++/95232
// { dg-additional-options "-Wno-vla -ftrapv -fnon-call-exceptions -O -fsanitize=undefined" }

template <typename T>
int tmain(T argc) {
  typedef double (*chunk_t)[argc[0][0]];
  chunk_t var;
  (void)var[0][0];
  return 0;
}

int main (int argc, char **argv) {
  return tmain(argv);
}
