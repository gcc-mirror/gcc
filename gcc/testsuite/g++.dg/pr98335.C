/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct Data {
  char a;
  int b;
};

char c;

Data val(int idx) {
  return { c };  // { dg-warning "extended initializer" "c++ 98"  { target { c++98_only } } }
}

/* { dg-final { scan-tree-dump-not " + 1B] = {}" "optimized" } } */
