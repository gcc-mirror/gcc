// PR c++/92032 - DR 1601: Promotion of enumeration with fixed underlying type.
// { dg-do compile { target c++11 } }

enum E1 : long { e1 };
enum E2 : short { e2 };

int f1(short);
void f1(int);

void f2(int);
int f2(short);

void f3(int);
int f3(long);

int f4(short);
void f4(long);

int f5(int);
void f5(long);

int f6(unsigned int); // { dg-message "candidate" }
void f6(long); // { dg-message "candidate" }

void
fn ()
{
  int r = 0;
  r += f1 (e2);
  r += f2 (e2);
  r += f3 (e1);
  r += f4 (e2);
  r += f5 (e2);
  r += f6 (e2); // { dg-error "ambiguous" }
}
