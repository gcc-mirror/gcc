// { dg-do assemble  }
// From: chw@bellcore.com (Charlie Woloszynski,MRE 2J-278,8295228,,27143)
// Newsgroups: gnu.g++.bug
// Subject: gcc-2.5.5 bug in multiple inheritance and pure virtual functions
// Date: 25 Jan 1994 23:41:36 -0500

// Bug: g++ fails to notice definitions of abstract virtuals.

class A 
{
public:
  virtual void a1() = 0;
  virtual void a2() = 0;
};

class B
{
public:
  virtual void b1() = 0;
  virtual void b2() = 0;
};


class C: public A, public B
{
public:
  virtual void a2() {};
  virtual void b2() {};
};

class D : public C
{
public:
  virtual void a1() {};
  virtual void b1() {};
};

int main()
{
  D d;				// { dg-bogus "" } 
}
