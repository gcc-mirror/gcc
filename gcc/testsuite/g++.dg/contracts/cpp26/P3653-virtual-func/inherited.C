// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontracts-on-virtual-functions=P3653 -g3" }
#include <cassert>

struct contract
{
  int checked = 0;
};

contract a, b, c;

bool
checkA ()
{
  a.checked++;
  return true;
}

bool
checkB ()
{
  b.checked++;
  return true;
}

bool
checkC ()
{
  c.checked++;
  return true;
}

void
clear_checks ()
{
  a.checked = b.checked = c.checked = 0;

}

struct Base1
{
  virtual int f1(const int i) post (checkA()){ return 1;};
  virtual int f2(const int i) pre (checkA()) post (checkB()){ return 1;};

};



struct Base2
{
  virtual int f1(const int i) pre (checkB()){ return 1;};
  virtual int f2(const int i) { return 1;};

};

struct Base3
{
  virtual int f1(const int i) pre (checkB()) pre (checkC()){ return 1;};
  virtual int f2(const int i) { return 1;};

};


struct Child0 : Base1
{
  virtual int f1(const int i) pre inherited (Base1) pre(checkC());
  virtual int f2(const int i) post inherited (Base1){ return 1;}
};

struct Child1 : Base1, private Base2, protected Base3
{
  virtual int f1(const int i) pre inherited (Base1, Base2, Base3){ return 1;}
  virtual int f2(const int i) post inherited (Base1, Base2, Base3){ return 1;}
};

struct GChild : Child0
{
  virtual int f1(const int i){ return 1;}
  virtual int f2(const int i) post inherited (Child0){ return 1;}
};

int Child0::f1(const int i){ return 3;}

int main()
{

  Child0 c0;

  clear_checks ();
  c0.f1(2);
  assert (a.checked == 0);
  assert (b.checked == 0);
  assert (c.checked > 0);

  clear_checks ();
  c0.f2(2);
  assert (a.checked == 0);
  assert (b.checked > 0);
  assert (c.checked == 0);


  Child1 c1;

  clear_checks ();
  c1.f1(2);
  assert (a.checked == 0);
  assert (b.checked > 0);
  assert (c.checked > 0);

  clear_checks ();
  c1.f2(2);
  assert (a.checked == 0);
  assert (b.checked > 0);
  assert (c.checked == 0);

  GChild g;

  clear_checks ();
  g.f1(2);
  assert (a.checked == 0);
  assert (b.checked == 0);
  assert (c.checked > 0);

  clear_checks ();
  c1.f2(2);
  assert (a.checked == 0);
  assert (b.checked > 0);
  assert (c.checked == 0);

}
