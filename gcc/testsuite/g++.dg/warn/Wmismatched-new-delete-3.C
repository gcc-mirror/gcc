/* PR c++/98305 spurious -Wmismatched-new-delete on template instance
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __SIZE_TYPE__ size_t;

void sink (void*, ...);

template <class>
struct A1
{
  A1 ();

  void* operator new (size_t);
  void operator delete (void*);

  void* operator new[] (size_t);
  void operator delete[] (void*);

  template <class, class>
  struct A2
  {
    A2 ();

    void* operator new (size_t);
    void operator delete (void*);

    void* operator new[] (size_t);
    void operator delete[] (void*);
  };
};

void test_a1 ()
{
  {
    A1<int> *p = new A1<int>();
    sink (p);
    delete p;
  }

  {
    A1<long> *p = new A1<long>();
    sink (p);
    delete p;
  }

  {
    void *p = new A1<int>();
    A1<long> *q = (A1<long>*)p;
    sink (q);
    delete q;       // { dg-warning "-Wmismatched-new-delete" }
  }
}

void test_a2 ()
{
  {
    A1<int>::A2<int, int> *p = new A1<int>::A2<int, int>();
    sink (p);
    delete p;
  }
  {
    A1<void>::A2<int, long> *p = new A1<void>::A2<int, long>();
    sink (p);
    delete p;
  }
  {
    A1<char*>::A2<long, double> *p = new A1<char*>::A2<long, double>();
    sink (p);
    delete p;
  }

  typedef A1<char>::A2<char, char> A;
  {
    A *p = (A*)new A1<char>::A2<char, int>();
    sink (p);
    delete p;       // { dg-warning "-Wmismatched-new-delete" }
  }

  {
    A *p = (A*)new A1<char>::A2<int, char>();
    sink (p);
    delete p;       // { dg-warning "-Wmismatched-new-delete" }
  }

  {
    A *p = (A*)new A1<int>::A2<char, char>();
    sink (p);
    delete p;       // { dg-warning "-Wmismatched-new-delete" }
  }
}


template <class>
struct B1
{
  B1 ();

  void* operator new (size_t);
  void operator delete (void*);

  void* operator new[] (size_t);
  void operator delete[] (void*);

  template <class, class>
  struct B2
  {
    B2 ();

    void* operator new (size_t);
    void operator delete (void*);

    void* operator new[] (size_t);
    void operator delete[] (void*);
  };
};


void test_b_b ()
{
  typedef B1<char> B1c;
  typedef B1c::B2<B1c, B1c> B1cB2B1c;

  {
    B1cB2B1c *p = new B1cB2B1c;
    sink (p);
    delete p;
  }

  {
    B1cB2B1c *p = new B1cB2B1c[1];
    sink (p);
    delete[] p;
  }
}


void test_a_b ()
{
  typedef B1<char>::B2<char, char> B;

  {
    B *p = (B*)new A1<char>::A2<char, int>[1];
    sink (p);
    delete[] p;     // { dg-warning "-Wmismatched-new-delete" }
  }

  {
    B *p = (B*)new A1<char>::A2<int, char>[2];
    sink (p);
    delete[] p;     // { dg-warning "-Wmismatched-new-delete" }
  }

  {
    B *p = (B*)new A1<int>::A2<char, char>[3];
    sink (p);
    delete[] p;     // { dg-warning "-Wmismatched-new-delete" }
  }
}
