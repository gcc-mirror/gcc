// { dg-do compile { target correct_iso_cpp_string_wchar_protos } }
// { dg-options "-O2 -fdump-tree-optimized" }
// { dg-skip-if "requires hosted libstdc++ for cstring" { ! hostedlib } }

#include <cstring>

const void *cv1;
const char *cc1, *cc2, *cc3, *cc4;
void *v1;
char *c1, *c2, *c3, *c4;

void
f1 (void)
{
  cv1 = memchr ("abcba", 'b', 3);
  cc1 = strchr ("abcba", 'b');
  cc2 = strrchr ("abcba", 'b');
  cc3 = strpbrk ("dabc", "abc");
  cc4 = strstr ("aaabc", "abc");
}

void
f2 (void)
{
  cv1 = std::memchr ("abcba", 'b', 3);
  cc1 = std::strchr ("abcba", 'b');
  cc2 = std::strrchr ("abcba", 'b');
  cc3 = std::strpbrk ("dabc", "abc");
  cc4 = std::strstr ("aaabc", "abc");
}

void
f3 (void)
{
  v1 = memchr ((char *)"abcba", 'b', 3);
  c1 = strchr ((char *)"abcba", 'b');
  c2 = strrchr ((char *)"abcba", 'b');
  c3 = strpbrk ((char *)"dabc", "abc");
  c4 = strstr ((char *)"aaabc", "abc");
}

void
f4 (void)
{
  v1 = std::memchr ((char *)"abcba", 'b', 3);
  c1 = std::strchr ((char *)"abcba", 'b');
  c2 = std::strrchr ((char *)"abcba", 'b');
  c3 = std::strpbrk ((char *)"dabc", "abc");
  c4 = std::strstr ((char *)"aaabc", "abc");
}

// { dg-final { scan-tree-dump-not "memchr" "optimized" } }
// { dg-final { scan-tree-dump-not "strchr" "optimized" } }
// { dg-final { scan-tree-dump-not "strrchr" "optimized" } }
// { dg-final { scan-tree-dump-not "strpbrk" "optimized" } }
// { dg-final { scan-tree-dump-not "strstr" "optimized" } }
