// { dg-do compile { target c++11 } }

void foo()
{
  static void bar1();  // { dg-error "3:.static. specifier invalid" }
// { dg-error "3:cannot declare static function" "" { target *-*-* } .-1 }
  inline void bar2();  // { dg-error "3:.inline. specifier invalid" }
}

struct S
{
  virtual S();  // { dg-error "3:constructors cannot be declared .virtual." }
  constexpr int s = 1;  // { dg-error "3:non-static data member .s. declared .constexpr." }
  constexpr ~S();  // { dg-error "3:'constexpr' destructors only available with" "" { target c++17_down } }
};

typedef constexpr int my_int;  // { dg-error "9:.constexpr. cannot appear in a typedef declaration" }

union U
{
  virtual void foo();  // { dg-error "3:function .foo. declared .virtual. inside a union" }
};

struct T
{
  virtual void operator delete(void*);  // { dg-error "3:.operator delete. cannot be declared .virtual." }
};

void bar(constexpr int);  // { dg-error "10:a parameter cannot be declared .constexpr." }
