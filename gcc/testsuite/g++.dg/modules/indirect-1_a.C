// { dg-module-do run }

// indirect references to import.  Non-template cases

export module foo;
// { dg-module-bmi foo }

namespace foo {

  export int frob (int i)
  {
    return i;
  }


  export class X 
  {
    int i;

  public:
    X (int i) :i(i) { }
    operator int () const { return i; }
  };

  export class Y : public virtual X
  {
    int j;
  public:
    Y (int i, int j) : X(i), j(j){}
    virtual int frob () const;
  };

  int Y::frob () const
  {
    return *this + j;
  }

  export enum Plain {A, B, C, D};
  export enum class Scoped {A, B, C, D};
}
