// PRMS Id: 9930
// Test of -fexternal-templates hackery in new template code
// Special g++ options: -fexternal-templates

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

  class ONE<int>
  {
    public:
      static void func();
  };

  void ONE<int>::func()
  {
  }

  main()
  {
    ONE<char>::func();
    ONE<int>::func();

    return 0;
  }
