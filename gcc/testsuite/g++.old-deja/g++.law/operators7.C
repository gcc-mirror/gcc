// GROUPS passed operators
// opr-as file
// From: amichail@lambert.waterloo.edu (Amir Michail)
// Date:     Mon, 15 Jun 1992 19:41:37 GMT
// Subject:  inheretance bug
// Message-ID: <AMICHAIL.92Jun15144137@lambert.waterloo.edu>

#include <stdio.h>
class window {
 public:
  int k;
  virtual void inc() {}
};

class window_border : public virtual window {
 public:
  void inc () { k++; }
};

class container {
 public:
  window_border c;
#if 0
  container& operator = (const container& o) {
    this->c = o.c;
    return *this;
  }
#endif
};

int main() {
  container test, *test2;
  void *vp;
  test2 = new container;
  test.c.k = 34;
  vp = (window *)&test2->c;
  *test2 = test;
  test.c.k = 60;
  if (test2->c.k == 35
      && test.c.k == 60)
    printf ("PASS\n");
  else
    printf ("FAIL\n");
}
