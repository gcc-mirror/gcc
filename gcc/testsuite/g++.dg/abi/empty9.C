// { dg-do run { target i?86-*-* } }
// { dg-options "-w -fabi-version=0" }

struct E1 {};
struct E2 : public E1 {
  virtual void f ();
};
struct E3 : virtual public E1 {
};
struct S : public E2, virtual public E3 {
};

int main () {
  if (sizeof (S) != 12)
    return 1;
}
