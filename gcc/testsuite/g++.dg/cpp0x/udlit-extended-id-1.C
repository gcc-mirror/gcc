// { dg-do run { target c++11 } }
// { dg-additional-options "-Wno-error=normalized" }
// { dg-require-effective-target ucn }
#include <cstring>
#include <cstddef>
using namespace std;

constexpr unsigned long long operator "" _π (unsigned long long x)
{
  return 3 * x;
}

/* Historically we didn't parse properly as part of the "" token, so check that
   as well.  */
constexpr unsigned long long operator ""_Π2 (unsigned long long x)
{
  return 4 * x;
}

char x1[1_π];
char x2[2_Π2];

static_assert (sizeof x1 == 3, "test1");
static_assert (sizeof x2 == 8, "test2");

const char * operator "" _1σ (const char *s, size_t)
{
  return s + 1;
}

const char * operator ""_Σ2 (const char *s, size_t)
{
  return s + 2;
}

const char * operator "" _\U000000e61 (const char *s, size_t)
{
  return "ae";
}

const char* operator ""_\u01532 (const char *s, size_t)
{
  return "oe";
}

bool operator "" _\u0BC7\u0BBE (unsigned long long); // { dg-warning "not in NFC" }
bool operator ""_\u0B47\U00000B3E (unsigned long long); // { dg-warning "not in NFC" }

#define xτy
const char * str = ""xτy; // { dg-warning "invalid suffix on literal" }

int main()
{
  if (3_π != 9)
    __builtin_abort ();
  if (4_Π2 != 16)
    __builtin_abort ();
  if (strcmp ("abc"_1σ, "bc"))
    __builtin_abort ();
  if (strcmp ("abcd"_Σ2, "cd"))
    __builtin_abort ();
  if (strcmp (R"(abcdef)"_1σ, "bcdef"))
    __builtin_abort ();
  if (strcmp (R"(abc
def)"_1σ, "bc\ndef"))
    __builtin_abort ();
  if (strcmp (R"(abcdef)"_Σ2, "cdef"))
    __builtin_abort ();
  if (strcmp ("xyz"_æ1, "ae"))
    __builtin_abort ();
  if (strcmp ("xyz"_œ2, "oe"))
    __builtin_abort ();
}
