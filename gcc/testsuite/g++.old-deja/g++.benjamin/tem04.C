// { dg-do assemble  }
// 980827 bkoz
// template parameter redeclaration bugs, part two:
// template template params and expanded template non-type parms

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
// same name as the template name.

// 14 
// declared friend template (v3, template type parameters)
template <class T4>// { dg-error "" } .*
class Xfourteen {
protected:
  T4 value;
public:
  Xfourteen(T4 init): value(init) {}
  template <template <typename T4> class T5> // { dg-error "" } .*
  friend bool isequal (Xfourteen<int>& lhs, Xfourteen<int>& rhs);
};


// 15
// nested template class (v3, template type parameters)
template <class T6>// { dg-error "" } .*
class Xfifteen {
protected:
  T6 value;
public:
  Xfifteen(T6 init): value(init) {}

  template <template <typename T6> class T7> class nested {// { dg-error "" } .*
    int value;
  public:
    nested(): value( int(0)) {}
  };
};


// 16
// member templates (v3, template type parameters)
template <class T8>// { dg-error "" } .*
class Xsixteen {
protected:
  T8 value;
public:
  Xsixteen(T8 init): value(init) {}

  template <template <typename T8> class T9> int comp_ge(int test) {// { dg-error "" } .*
    int local_value;
    if (local_value > value) 
      return local_value;
    else
      return value;
  }
};


// 17
// declared friend template (v4, template type parameters on the class)
template <typename T9> class tem_base {
public:
  T9 value;
};

template <typename T10, template <typename T12> class C10>
class Xseventeen {
protected:
  C10<T10> value;
public:
  Xseventeen(){}
  template <typename T12> // ok??
  friend bool isequal (Xseventeen<T10, tem_base>& lhs, 
		       Xseventeen<T10, tem_base>& rhs);
};

//template class Xseventeen<int, tem_base>;


// 18
// more template template redecl tests
template <typename T14, template <typename T15> class C12>// { dg-error "" } .*
class Xeighteen {
protected:
  C12<T14> value; // { dg-error "" }
  int C12; // { dg-error "" } .*
};


// 19
// more template template redecl tests
template <typename T16, template <typename T17> class C14>// { dg-error "" } .*
class Xnineteen{
protected:
  C14<T16> value;
  template <class C14> class nested {// { dg-error "" } .*
    T16 value;
  public:
    nested(): value( T16(0)) {}
  };
};


// 20
// local names (14.6.1 p 4) part two, variable names as template param
template <class T17, int i> struct Xtwenty {
  void f(){
    T17 my_type; //ok
    for (int j = 0; j < 5; ++j)
      {
	T17 my_type; //ok
	++my_type;
      }
  }
};


// 14.1 Template parameters
// p 4
// A non-type templat- parameter shall have one of the following
// (optionally cv-qualified) types:
//   integral or enumeration type
//   pointer to object or pointer to function
//   referenct to object or referece to function
//   pointer to member

// 21 
// non-type template parameters v1: enum
enum my_enum {my_A = 45, my_B, my_C};

template <my_enum T18> class Xtwentyone {// { dg-error "" } .*
  float T18; // { dg-error "" } .*
};


// 22
// non-type template parameters v1: pointer to object
struct base {
  int	gcount;
  int ret_gcount() {return gcount;}
};

template <class T20, base* b> class Xtwentytwo {// { dg-error "" } .*
  float b; // { dg-error "" } .*
};


// 23
// non-type template parameters v2: reference to object
template <class T20, base& b2> class Xtwentythree {// { dg-error "" } .*
  float b2; // { dg-error "" } .*
};


// 24
// non-type template parameters v3: pointer to member
template <class T20, int base::* b3> class Xtwentyfour {// { dg-error "" } .*
  float b3; // { dg-error "" } .*
};


// 25
// non-type template parms that use push_class_level
template <class T22> void f1() {// { dg-error "" } .*
  struct foo { 
    enum T22 { un, du, toi }; // { dg-error "" } .*
  };
}





