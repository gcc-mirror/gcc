// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/42260
// { dg-do compile }

struct A
{
      template<typename T> operator T*();
};

int i = *A();// { dg-error "no match" }
