/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct ExtentsBase {
 ExtentsBase() : startx_(), endx_() { }
 ExtentsBase(const ExtentsBase &b) {
  *this = b;
 }

 const ExtentsBase & operator=(const ExtentsBase &b) {
  if (this != &b) {
    startx_ = b.startx_;
  }
  return *this;
 }

 int startx_;
  int endx_;
};

int f(const ExtentsBase &e1) {
 ExtentsBase my_extents = e1;
 return my_extents.startx_;
}

/* { dg-final { scan-tree-dump-not "&my_extents" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

