// Build don't link:

class A {
};


class B {
public:
  void f();

private:
  B (); 
  B (const B&);
}; // WARNING - only private constructors


class C { 
public: 
  void f();

private:
  ~C (); 
}; // WARNING - only private destructors


class D {
private:
  void f(); 
}; // WARNING - all member functions are private


template <class T>
class X {
private:
  ~X (); 
}; // WARNING - only private destructors

template class X<int>;
template class X<double>;


template <class T>
class Y {
private:
  Y (); 
  Y (const Y&);
}; // WARNING - only private constructors


template <class T>
class Z {
private:
  void f(); 
}; // WARNING - all member functions are private
