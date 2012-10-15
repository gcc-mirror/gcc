// PR c++/24449

class Fooa
{
  friend int main();
};

template <class T> class Foob
{
  friend int main();
  int i;
};

int main()
{
  Foob<void> a;
  a.i = 7;
}

class Fooc
{
  template<class T> friend int main(); // { dg-error "cannot declare .::main. to be a template" }
};

template<class T> class Food
{
  template<class U> friend int main(); // { dg-error "cannot declare .::main. to be a template" }
};

template<class U> int main() {} // { dg-error "cannot declare .::main. to be a template" }
