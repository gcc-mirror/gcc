// { dg-do run }
// { dg-options "-fabi-version=0 -w" }

struct E1 {};
struct E2 : public E1 {};
struct E : public E1, public E2 {};
struct N : public E { virtual void f () {} };

struct X : virtual public N {
};

int main () {
  X x;
  /* N should not be the primary base of X; it is not nearly empty.  */
  if ((void*)&x == (void*)(N*)&x)
    return 1;
}
