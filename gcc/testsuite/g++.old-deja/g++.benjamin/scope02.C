// { dg-do assemble  }
//980529 bkoz
//3.4.5 Class member access via pointer and non-pointer
// non-nested dtor calls

int counter = 0;

struct X {
  int rank;
  X(int init = 64) : rank(init) { }
  ~X() { ++counter; }
  typedef X classtype;
};
typedef X globaltype;

#if 0
template <typename T>
struct X_tem {
  T rank;
  X_tem(T init = T(64) ) : rank(init) { }
  ~X_tem() { ++counter; }
  typedef X_tem classtype_tem;
};
typedef X_tem<int> globaltype_tem;
#endif




int main(void)
{
  // 3.4.5 Class member access
  // p 2
  // if the id-expression in a class member access is an
  // unqualified-id, and the type of the object expression is of class
  // type C (or pointer to class type C), the unqualified-id is looked
  // up in the scope of class C. If the type of the object-expression
  // is of pointer to scalar type, the unqualified-id is looked up in
  // the context of the complete postfix-expression.

  // p 3
  // if the unqualitified id is ~type-name, and the type of the object
  // expression is of a class type C (or pointer to class type C), the
  // type-name is looked up in the context of the entire
  // postfix-expression and in the scope of class C. The type-name
  // shall refer to a class-name. If type-name is found in both
  // contexts, the name shall refer to the same class type. If the
  // type of the object expression is of scalar type, the type-name is
  // looked up in the complete postfix-expression.
  
  typedef X localtype;

  //
  // 1 non-templatized, pointer, unqualified
  //
  X x01 ;
  X *px = &x01;
  px->~X(); 

  X x02 (66);
  px = &x02;
  px->~localtype();

  X x03 (68);
  px = &x03;
  px->~classtype(); //-g++  //p3: unqual-id lookup in object and postfix-expr

  X x04 (70);
  px = &x04;
  px->~globaltype();


  // p 1
  // . . . the id-expression is first looked up in the class of the
  // object-expression. If the identifier is not found, itis then
  // looked up in the context of the entier postfix-expression and
  // shall name a class or function template. If the lookup in the
  // class of the object-expression finds a template, the name is also
  // looked up in teh context of the entier postfix-expression and
  // 1 if the name is not found, use the name from the object-expr
  // 2 if the name found in postfix-expr != class template, use object-expr
  // 3 if name found is class template, name must match object-expr or error

  // p 4 

  // if the id-expr in a class member acess is a qualified-id, the
  // id-expression is looked up in both the context of the entire
  // postfix-expr and in the scope of the class of the object-expr. If
  // the name is found in both contexts, the id-expr shall refer to
  // the same entity.


  //
  // 2 non-templatized, pointer, qualified
  //
  X x05 ;
  px = &x05;
  px->X::~X(); 

  X x06 (66);
  px = &x06;
  px->X::~localtype();

  X x07 (68);
  px = &x07;
  px->X::~classtype(); // -edg

  X x08 (70);
  px = &x08;
  px->X::~globaltype();

  X x09 (66);
  px = &x09;
  px->localtype::~localtype();

  X x10 (68);
  px = &x10;
  px->classtype::~classtype();

  X x11 (70);
  px = &x11;
  px->globaltype::~globaltype();

  X x12 (66);
  px = &x12;
  px->classtype::~localtype();

  X x13 (68);
  px = &x13;
  px->globaltype::~localtype();

  X x14 (70);
  px = &x14;
  px->localtype::~globaltype();

  X x15 (70);
  px = &x15;
  px->classtype::~globaltype();

  X x16 (70);
  px = &x16;
  px->localtype::~classtype(); //-edg

  X x17 (70);
  px = &x17;
  px->globaltype::~classtype(); //-edg

#if 0
  //
  // non-templatized, non-pointer
  //
  X xo5 ;
  xo5.~X(); //unqualified

  localtype xo6 (66);
  xo6.~localtype();

  X xo7 (68);
  xo7.~classtype();

  X xo8 (70);
  xo8.~globaltype();


  //
  // templatized, pointer
  //
  X_tem<int> xto1 ;
  X_tem<int> *pxt = &xto1;
  pxt->~X_tem(); //unqualified

  typedef X_tem<int> localtype_tem;
  localtype_tem xto2 (66);
  pxt = &xto2;
  pxt->~localtype_tem();

  //paragraph 2:  unqualitifed id looked up in scope of post-fix expr if object
  X_tem<int> xto3 (68);
  pxt = &xto3;
  pxt->~classtype_tem();

  X_tem<int> xto4 (70);
  pxt = &xto4;
  pxt->~globaltype_tem();

  //
  // templatized, non-pointer
  //
  X_tem<int> xto5 ;
  xto5.~X_tem(); //unqualified

  localtype_tem xto6 (66);
  xto6.~localtype_tem();

  X_tem<int> xto7 (68);
  xto7.~classtype_tem();

  X_tem<int> xto8 (70);
  xto8.~globaltype_tem();
#endif
  return 0;
}






