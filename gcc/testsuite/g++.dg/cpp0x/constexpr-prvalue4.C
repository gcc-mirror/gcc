// PR c++/118396
// { dg-do run { target c++11 } }
// { dg-options "-O" }

void *operator new(__SIZE_TYPE__, void *__p) { return __p; }

struct Foo {
  virtual ~Foo() = default;
};
struct Data {
  int status;
  Foo data{};
};

Data *P, *Q;

struct vector {
  vector (const Data &__value) {
    P = static_cast<Data *>(__builtin_operator_new(0));
    new (P) Data (__value);
    Q = P + 1;
  }
  Data *begin() { return P; }
  Data *end() { return Q; }
};

int
main ()
{
  vector items_(Data{});
  for (auto item : items_)
    item.status == 0 ? void() : __builtin_abort ();
}
