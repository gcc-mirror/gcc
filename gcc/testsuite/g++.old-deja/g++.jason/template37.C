// { dg-do run  }
// { dg-options "-Wno-deprecated -fexternal-templates" }
// { dg-warning "switch.*deprecated" "" { target *-*-* } 0 }

// PRMS Id: 9930
// Test of -fexternal-templates hackery in new template code

  #pragma implementation "foo.hh"
  #pragma interface "foo.hh"

  template<class T>
  class ONE
  {
    public:
      static void func();
  };

  template<class T>
  void ONE<T>::func()
  {
  }

  template <>
  class ONE<int>
  {
    public:
      static void func();
  };

  void ONE<int>::func()
  {
  }

int main()
  {
    ONE<char>::func();
    ONE<int>::func();

    return 0;
  }
