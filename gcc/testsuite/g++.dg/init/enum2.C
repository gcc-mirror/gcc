#include <limits.h>
enum test {
  z = 0,
  c = UINT_MAX + 1LL
} x = z;

int main() {
  return x != z;
}
