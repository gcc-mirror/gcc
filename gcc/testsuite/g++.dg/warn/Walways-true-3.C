// PR c++/65168
// { dg-options "-Waddress" }

void foo (void);

int d;
int &c = d;

void
bar (int &a)
{
  int &b = a;

  if ((int *)&a) // { dg-warning "7:the compiler can assume that the address of" }
    foo ();

  if (&b) // { dg-warning "7:the compiler can assume that the address of" }
    foo ();

  if (!&c) // { dg-warning "8:the compiler can assume that the address of" }
    foo ();

  if (!&(int &)(int &)a) // { dg-warning "8:the compiler can assume that the address of" }
    foo ();

  if (&a == 0) // { dg-warning "10:the compiler can assume that the address of" }
    foo ();

  if (&b != 0) // { dg-warning "10:the compiler can assume that the address of" }
    foo ();

  if (0 == &(int &)(int &)c) // { dg-warning "9:the compiler can assume that the address of" }
    foo ();

  if (&a != (int *)0) // { dg-warning "10:the compiler can assume that the address of" }
    foo ();
}

bool
bar_1 (int &a)
{
  if (d == 5)
    return &a; // { dg-warning "12:the compiler can assume that the address of" }
  else
    return !&(int &)(int &)a; // { dg-warning "13:the compiler can assume that the address of" }
}
