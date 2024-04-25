// { dg-lto-do link }
// { dg-lto-options { {-O1 -std=c++20 -flto}} }
// { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" }
// { dg-require-linker-plugin "" }

#define CONSTEXPR constexpr
#include "pr113208.h"

struct QualityValue;
struct k : vector<QualityValue> {};

void m(k);
void n(k i) { m(i); }
