// PR c++/11224
// { dg-options "-Wunused" }

struct someclass {

  bool isEmpty() const { return true; }
};

int main()
{
  bool bOk = false;
  someclass foo;

  bOk == foo.isEmpty(); // { dg-warning "not used" }

  return bOk;
}

int& f();

void g() {
  f() == 0; // { dg-warning "not used" }
  f() != 1; // { dg-warning "not used" }
  f() < 2; // { dg-warning "not used" }
  f() > 3; // { dg-warning "not used" }
  f() <= 4; // { dg-warning "not used" }
  f() >= 5; // { dg-warning "not used" }
  f() + 6; // { dg-warning "not used" }
  f() - 7; // { dg-warning "not used" }
  f() * 8; // { dg-warning "not used" }
  f() / 9; // { dg-warning "not used" }
  +f(); // { dg-warning "not used" }
  -f(); // { dg-warning "not used" }
  ++f();
  --f();
  f() = 10;
  f() <<= 11; 
}
