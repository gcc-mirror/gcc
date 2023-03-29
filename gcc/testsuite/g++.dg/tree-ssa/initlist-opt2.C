// PR c++/105838
// { dg-additional-options -fdump-tree-gimple }
// { dg-do compile { target c++11 } }

// Test that we do range-initialization from const char *.
// { dg-final { scan-tree-dump {_M_range_initialize<const char\* const\*>} "gimple" } }

#include <string>
#include <vector>

void g (const void *);

template <int N>
void f (const char *p)
{
  std::vector<std::string> lst = {
  "aahing", "aaliis", "aarrgh", "abacas", "abacus", "abakas", "abamps", "abands", "abased", "abaser", "abases", "abasia",
  "abated", "abater", "abates", "abatis", "abator", "abattu", "abayas", "abbacy", "abbess", "abbeys", "abbots", "abcees",
  "abdabs", "abduce", "abduct", "abears", "abeigh", "abeles", "abelia", "abends", "abhors", "abided", "abider", "abides",
  "abject", "abjure", "ablate", "ablaut", "ablaze", "ablest", "ablets", "abling", "ablins", "abloom", "ablush", "abmhos",
  "aboard", "aboded", "abodes", "abohms", "abolla", "abomas", "aboral", "abords", "aborne", "aborts", "abound", "abouts",
  "aboves", "abrade", "abraid", "abrash", "abrays", "abrazo", "abrege", "abrins", "abroad", "abrupt", "abseil", "absent",
  };

  g(&lst);
}

void h (const char *p)
{
  f<0> (p);
}
