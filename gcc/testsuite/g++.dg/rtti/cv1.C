// { dg-do run }

#include <typeinfo>
#include <string.h>

struct S {};

typedef S volatile T[4];

T t[3];

const std::type_info& ti = typeid (t);

int main () {
  if (strcmp (ti.name (), "A3_A4_1S") != 0)
    return 1;
}
