/* { dg-do run } */
/* { dg-options "-O2" } */

#include <cstdio>
#include <string>

std::string __attribute__ ((noinline)) comp_test_write() {
  std::string data;

  for (int i = 0; i < 2; ++i) {
    char b = 1 >> (i * 8);
    data.append(&b, 1);
  }

  return data;
}

std::string __attribute__ ((noinline)) comp_test_write_good() {
  std::string data;

  char b;
  for (int i = 0; i < 2; ++i) {
    b = 1 >> (i * 8);
    data.append(&b, 1);
  }

  return data;
}

int main() {
  std::string good = comp_test_write_good();
  printf("expected: %hx\n", *(short*)good.c_str());

  std::string bad = comp_test_write();
  printf("got: %hx\n", *(short*)bad.c_str());

  return good != bad;
}
