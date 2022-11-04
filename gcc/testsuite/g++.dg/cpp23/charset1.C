// P2314R4
// { dg-do compile { target c++23 } }
// { dg-options "-finput-charset=UTF-8 -fexec-charset=UTF-8" }

#define S(x) # x
const char s1[] = S(Köppe);       // "Köppe"
const char s2[] = S(K\u00f6ppe);  // "Köppe"

static_assert (sizeof (s1) == 7);
static_assert (sizeof (s2) == 7);
