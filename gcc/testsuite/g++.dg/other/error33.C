// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/42218
// { dg-do compile }

template<int> struct A
{
      template<int> struct B;
};

int i = A<0>::B<0>::X::Y; // { dg-error "'A<0>::B<0>::X' has not been declared" }

