// { dg-do assemble  }
// From: panisset@cae.ca (Jean-Francois Panisset)
// Date: Mon, 6 Jun 94 13:39:25 EDT
// Subject: Problem with operator overloading


class ostream {
public:
  ostream& operator<<(double n);
  ostream& operator<<(float n);
};

class X
{
public:
  operator long() const;
  operator double() const;
};
ostream& operator<< (ostream& os, const X& x);


int main()
{
  X x;
  ostream os;
  os << x; // { dg-bogus "" } converting to float
}
