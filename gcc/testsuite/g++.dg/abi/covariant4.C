// { dg-do run  }

// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 11 Feb 2005 <nathan@codesourcery.com>

// Origin: bredelin@ucla.edu
// Bug 19891: Incorrect covariant vtables

struct Model {
  bool full_tree;
  virtual Model* clone() const =0;
  virtual const char *name() const =0;
  virtual ~Model() {}
};

struct R: virtual public Model {
  virtual R* clone() const =0;
};
struct A: virtual public Model {
  virtual A* clone() const=0;
};
struct RA: public R, public A {
  virtual RA* clone() const=0;
};

static const char *string = "EQU";

struct EQU: public RA {
  virtual EQU* clone() const {return new EQU(*this);}
  const char *name() const {return string;}
};

int main() {
  Model* M1 = new EQU();
  Model* M2 = M1->clone();
  Model* M3 = M2->clone();

  if (M1->name () != string)
    return 1;
  if (M2->name () != string)
    return 2;
  if (M3->name () != string)
    return 3;
  
  return 0;
}
