// Special g++ Options: -w
// GROUPS passed constructors
// ctors file
// Message-Id: <ACHILLES.92Nov25192123@i90s8.ira.uka.de>
// From: Alf-Christian Achilles <achilles@ira.uka.de>
// Subject: g++ 2.3.1 rejects initialization with object of derived class
// Date: 25 Nov 92 19:21:23

extern "C" int printf (const char *, ...);

class A {
public:
  virtual void foo() {};
};

class B : public A {
public:
  void foo() {};
};

main ()
{
  B b;
  A a = b;  //unjustified error: unexpected argument to constructor `A'
  printf ("PASS\n");
}

