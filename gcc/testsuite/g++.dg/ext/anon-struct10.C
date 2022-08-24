// PR c++/101767
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-pedantic" }

typedef struct {
  struct {
    int x;
  };
  union {
    int y;
    float z;
  };
} S;

void foo(void)
{
  [[maybe_unused]] S a = {
    .x = 1,
    .y = 0
  };
}
