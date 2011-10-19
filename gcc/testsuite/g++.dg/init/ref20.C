// PR c++/50787
// { dg-do run }

int main() {
  const int Ci = 0;
  const int &rCi = Ci;
  if (!(&Ci == &rCi)) __builtin_abort();
}
