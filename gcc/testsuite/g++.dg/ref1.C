// Origin: Peter Schmid <schmid@snake.iap.physik.tu-darmstadt.de>

// { dg-do link }

template <class T>
class Ptr {
protected:
  T * ptr;

public:
  
  Ptr(void) : ptr(0) { };
  Ptr(T * p) : ptr(p) { };
  
  ~Ptr(void) { delete ptr; }
  
  operator T & () { return *ptr; }
};

class base {
public: 
  base(void) { }
  ~base(void) { }
};


class foo : public base {
private:
  foo(const foo & rv);
  
public:
  
  foo(void) { }
  ~foo(void) { }
};

void func2(base & b) {
  // ...
}

int main () {
  Ptr<foo> f = new foo;
  /* This should not result in a copy; the result of the conversion
     operator should be bound directly to the reference argument to
     `func2'.  */
  func2(f);
}
