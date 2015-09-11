// PR c++/65339
// { dg-do compile { target c++11 } }

class FuncWrapper {
public:
  template <typename Func> void callfunc(Func f)
  {
     f();
  }
};

class Object {
  int field;
public:
  void Method();
  Object() { field = 555; }
  Object(const Object&) { __builtin_abort(); }
};

void Object::Method ()
{
  FuncWrapper wrap;
  wrap.callfunc(*[]()
		{
		  return Object();
		});
}
