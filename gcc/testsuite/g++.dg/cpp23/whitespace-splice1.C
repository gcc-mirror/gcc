// Tests from P2223R2
// { dg-additional-options -w }
// { dg-do compile { target c++11 } }

constexpr auto str = "\ 
";

static_assert(__builtin_strlen(str) == 0, "");

constexpr int i = 1
  // \ 
  + 42
  ;

static_assert(i == 1, "");
