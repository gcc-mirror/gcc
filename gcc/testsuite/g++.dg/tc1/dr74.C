// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR74: Enumeration value in direct-new-declarator 

struct A {};
enum E1 { COUNT = 10 };

A* a = new A[COUNT];

