// Build don't link:

class A {
};


class B { // WARNING - only private constructors
public:
  void f();

private:
  B (); 
  B (const B&);
};


class C {  // WARNING - only private destructors
public: 
  void f();

private:
  ~C (); 
};


class D { // WARNING - all member functions are private
private:
  void f(); 
};


template <class T>
class X { // WARNING - only private destructors
private:
  ~X (); 
};

template class X<int>;
template class X<double>;


template <class T>
class Y { // WARNING - only private constructors
private:
  Y (); 
  Y (const Y&);
};


template <class T>
class Z { // WARNING - all member functions are private
private:
  void f(); 
};
