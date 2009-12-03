// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/42217
// { dg-do compile }

struct A
{
 int : 0;
};
A a = A();

