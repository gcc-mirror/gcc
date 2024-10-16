/* { dg-options "-std=c23 --embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" } */

const unsigned char a[] = {
  #embed <magna-carta.txt> prefix ([0] = ) suffix (,)
  [256] = 42
};
