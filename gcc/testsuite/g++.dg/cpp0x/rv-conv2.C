// PR c++/89705
// { dg-do compile { target c++11 } }

struct W { operator const volatile int(); };
const int& rci = W();

struct X { operator const int(); };
int&& rri = X();

struct Y { operator volatile int(); };
int&& rri2 = Y();

struct Z { operator const volatile int(); };
volatile int&& rri3 = Z();

enum E { A };
struct S { operator const E(); };
E&& rre = S();
