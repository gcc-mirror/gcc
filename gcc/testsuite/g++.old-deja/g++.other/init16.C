// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

// excess errors test - XFAIL *-*-*

#include <string>

struct foo {
  string x;
};
extern const struct foo y = { "foo" };
