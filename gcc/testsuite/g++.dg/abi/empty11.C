// { dg-do run }
// { dg-options "-w -fabi-version=0" }

struct E {};
struct E2 : public E {};
struct E3 : public E, public E2 {};
struct E4 : public E, public E2, public E3 { };
struct E5 : public E, public E2, public E3, public E4 {};

struct S : public virtual E5 {
  E e;
};

S s;

int main () {
  if ((char*)(E4*)&s - (char*)&s == 0)
    return 1;
}
