// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

typedef enum {} i;

template <int II>
class Bar {};

void f (Bar<21>, int) {}
void f (Bar<2>, i) {}
