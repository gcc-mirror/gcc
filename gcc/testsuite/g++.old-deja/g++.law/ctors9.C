// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// GROUPS passed constructors
// ctors file
// Message-Id: <9301132030.AA05210@cs.rice.edu>
// From: dougm@cs.rice.edu (Doug Moore)
// Subject: 2.3.3: accepts ctor-less derived class of ctor-ful base class
// Date: Wed, 13 Jan 93 14:30:21 CST
// Note: It gives an error now.  But not a very good one.

struct Foo
{
  Foo(int aa);
  int a;
  const Foo* operator-> () const {return this;}
};

Foo::Foo(int aa)
:a(aa)
{ }


struct var_Foo: public Foo // { dg-message "note" }  base.*// ERROR -  in class.*
{
  var_Foo* operator-> () {return this;}
};

int blort(Foo& f)
{
  return f->a;
}

int main()
{
  var_Foo b(2);// { dg-error "match" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 35 }
  b->a = 0;
  int x = blort(b);
  return x;
}
