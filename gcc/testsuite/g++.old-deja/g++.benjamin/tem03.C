// Build don't link:
// 980808-980824 bkoz
// template parameter redeclaration bugs

// 14.1 Template parameters
// p 13
// The scope of a template-parameter extens from its point of
// declartion until the end of its template. In particular, a
// template-parameter can be used in the declaration of subsequent
// template-parameters and their default arguments. 

// 14.6.1 Locally declared names
// p 4
// A template-parameter shall not be redeclared within its scope
// (including nested scopes). A template-parameter shall not have the
// sname name as the template name.


// 01 
// declared friend template
template <class T4>// ERROR - .*
class Xone {
protected:
  T4* next;
  T4* prev;
  T4 value;
public:
  Xone(): next(0), prev(0), value(1999){}
  Xone(T4 init): value(init) {}

  // these are ok:
  // can also do template-decl and then can ditch the foward-declaration
  // template <class T5> friend bool isequal (Xone<T5>& lhs, Xone<T5>& rhs);
  // this is not ok:
  template <class T4> friend bool isequal (Xone<T4>& lhs, Xone<T4>& rhs);// ERROR - .*
};


// 02
// nested template class
template <class T6>// ERROR - .*
class Xtwo {
protected:
  T6* next;
  T6* prev;
  T6 value;
public:
  Xtwo(): next(0), prev(0), value(1999){}
  Xtwo(T6 init): value(init) {}

  template <class T6> class nested {// ERROR - .*
    T6 value;
  public:
    nested(): value( T6(0)) {}
  };
};


// 03
// member templates
template <class T8>// ERROR - .*
class Xthree {
protected:
  T8* next;
  T8* prev;
  T8 value;
public:
  Xthree(): next(0), prev(0), value(1999){}
  Xthree(T8 init): value(init) {}

  template <class T8> T8 comp_ge(T8 test) {// ERROR - .*
    T8 local_value;
    if (local_value > value) 
      return local_value;
    else
      return value;
  }
};


// 04
// local names (14.6.1 p 4)
template <class T10, int i> struct Xfour {// ERROR - .*
  int T10; // ERROR - .*
  void f(){
    char T10;
  }
};


// 05
// using different tempate-parms for out-of-line defs
template <class T12, int i> struct Xfive {
  void f();
};

template <class T13, int i> void Xfive<T13,i>::f() {// ERROR - .*
  int T13; // ERROR - .*
  int T12; //should be ok
}


// 06
// multiple names at the same level
template <class T14, class T14> class Xsix { // ERROR - .*
private:
public:
  void f();
};


// 07
// multiple names, one in template parameter one in class-name
template <class T12> class T12; // ERROR - .*


// 08 
// with multiple template params, and second (third) one is redeclared
template <class T16, int i, class T161> class Xseven { // ERROR - .*
private:
  char T161; // ERROR - .*
public:
  template <class U>
  friend bool fooy(U u);

  template <class T161>
  friend bool foo(T161 u)
    {
      Xseven<T161, 5, int> obj;
      return (obj.inst == u.inst);
    }

};


// 09 
// check for correct scoping of member templates
template <class T>
struct S1
{
  template <class U>
  void f(U u)
    {
      S1<U> s2u(u);
      s2u.g();
    }

  template <class U> //ok
  void f2(U u)
    {
      S1<U> s2u(u);
      s2u.g();
    }

};


// 10 
// check for non-type parameters, should still be able to redeclare?
// local names (14.6.1 p 4)
template <class T18, int i> class Xten {// ERROR - .*
  float i; // ERROR - .*
};


// 11 
// declared friend template, non-type parameters
template <long l>// ERROR - .*
class Xeleven {
public:
  template <long l> friend bool isequal (Xeleven<5> lhs, Xeleven<5> rhs);  // ERROR - .*
};



// 12
// nested template class, non-type parameters
template <long l>// ERROR - .*
class Xtwelve {
public:
  template <long l> class nested {// ERROR - .
    long value;
  public:
    nested(): value(0) {}
  };
};


// 13
// member templates, non-type parameters
template <long l>// ERROR - .*
struct Xthirteen {
  template <long l> long comp_ge(long test) {// ERROR - .
    long local_value;
    if (local_value > value) 
      return local_value;
    else
      return value;
  }
};









