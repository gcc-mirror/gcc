// { dg-do link  }
// Origin: scott snyder <snyder@fnal.gov>

struct Cleaner
{
  ~Cleaner() {}
};

template <class T>
void bar ()
{
  static Cleaner cleanup;
}


inline
void foo() { bar<int>(); }

int main () {}
