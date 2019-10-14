// PR c++/92032 - DR 1601: Promotion of enumeration with fixed underlying type.
// { dg-do compile { target c++11 } }

enum E : char { e };
enum F : int { f };
enum G : long { g };
enum H : unsigned { h };

int f1(char);
void f1(int);

void f2(int);
int f2(char);

int f3(int);
void f3(short);

int f4(long);
void f4(int);

void f5(unsigned);
int f5(int);

int f6(unsigned);
void f6(int);

void
test ()
{
  int r = 0;
  r += f1 (e);
  r += f2 (e);
  r += f3 (f);
  r += f4 (g);
  r += f5 (f);
  r += f6 (h);
}
