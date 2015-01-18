// { dg-do compile }
// { dg-options "-fno-ipa-cp" }
struct data {
  data(int) {}
};

struct top {
  virtual int topf() {}
};

struct child1: top {
    void childf()
    {
        data d(topf());
    }
};

void test(top *t)
{
    child1 *c = static_cast<child1 *>(t);
    c->childf();
    child1 d;
    test(&d);
}
