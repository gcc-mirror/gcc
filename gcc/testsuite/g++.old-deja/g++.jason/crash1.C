// { dg-do assemble  }
// Bug: g++ dies on this input.

class Sample
  {
 public:
  int operator <<(const char *c);
  };

extern Sample sample;

struct Simple
  {
  int a;
  };

extern "C" void get_it();

class Test
  {
 private:
  void test();
  friend void get_it();
  };

void Test::test()
  {
  sample << "hello";
  }
