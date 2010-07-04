// Test that we don't revert trigraphs and line splicing when a raw string
// literal is formed by token pasting.
// { dg-options "-std=gnu99 -trigraphs" { target c } }
// { dg-options "-std=c++0x" { target c++ } }
// { dg-do run }

#define PASTE(X,Y) X##Y

const char a[] = PASTE(R,"(??>\
)");

#define TEST(str, val) \
  if (sizeof (str) != sizeof (val) \
      || __builtin_memcmp (str, val, sizeof (str)) != 0) \
    __builtin_abort ()

int main()
{
  TEST (a, "}");
}
