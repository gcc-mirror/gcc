// { dg-do compile }
// { dg-options "-fgnu-tm" }

#define __ts	__attribute__((transaction_safe))
#define __tc	__attribute__((transaction_callable))
#define __tp	__attribute__((transaction_pure))
#define __tu	__attribute__((transaction_unsafe))

struct __ts A
{
  virtual void f();
  virtual void g();
};

struct __tc B : public A
{
  void f() __tc;  // { dg-error ".transaction_callable. overriding .transaction_safe." }
  void g();
  virtual void h();
};

struct C : public B
{
  void g() __tc;  // { dg-error ".transaction_callable. overriding .transaction_safe." }
};

struct C2 : public B
{
  void g() __ts;
  void h() __tu;  // { dg-error ".transaction_unsafe. overriding .transaction_callable." }
};

struct D
{
  virtual void f() __tp;
  virtual void g() __tp;
};

struct E : public D
{
  void f() __ts;  // { dg-error ".transaction_safe. overriding .transaction_pure." }
  void g();
};

struct F : public E
{
  void g() __ts;  // { dg-error ".transaction_safe. overriding .transaction_pure." }
};
