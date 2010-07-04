// Make sure that we properly handle trigraphs in raw strings when
// trigraphs are disabled, too.
// { dg-options "-std=gnu99" { target c } }
// { dg-options "-std=gnu++0x" { target c++ } }
// { dg-do run }

const char b[] = "??>";		// { dg-message "-trigraphs" }
const char a[] = R"(??>??)??/
??)";

#define TEST(str, val) \
  if (sizeof (str) != sizeof (val) \
      || __builtin_memcmp (str, val, sizeof (str)) != 0) \
    __builtin_abort ()

int main()
{
  TEST (a, "?\?>?\?)?\?/\n?\?");
}
