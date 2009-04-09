unsigned long f1();
int f2();

int store_aff_word(int x) {
  return (int) (x ? f1() : f2());
}
