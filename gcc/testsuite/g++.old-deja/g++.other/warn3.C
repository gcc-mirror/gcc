// { dg-do assemble  }
// { dg-options "-Wctor-dtor-privacy" }

class A {
};


class B { // { dg-warning "" } only private constructors
public:
  void f();

private:
  B (); 
  B (const B&);
};


class C {  // { dg-warning "" } only private destructors
public: 
  void f();

private:
  ~C (); 
};


class D { // { dg-warning "" } all member functions are private
private:
  void f(); 
};


template <class T>
class X { // { dg-warning "" } only private destructors
private:
  ~X (); 
};

template class X<int>;
template class X<double>;


template <class T>
class Y { // { dg-warning "" } only private constructors
private:
  Y (); 
  Y (const Y&);
};


template <class T>
class Z { // { dg-warning "" } all member functions are private
private:
  void f(); 
};
