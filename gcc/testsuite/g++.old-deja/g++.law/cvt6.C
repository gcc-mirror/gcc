// Build don't link: 
// GROUPS passed conversions
// cvt file
// Message-Id: <9208261851.AA16997@josquin.media.mit.edu>
// From: bilmes@media.mit.edu
// Subject: gcc (g++) 2.2.2 constructing nested class from external scope
// Date: Wed, 26 Aug 92 14:51:17 -0400


class foo {

  class bar {
    int i;
  public:
    bar(int j) { i = j; }
  };

  bar b;
public:
  foo() : b(3) {}
  void test(bar lb) { b = lb; }
};


int main() {
  foo f;
  f.test(34);  // line 18
}
