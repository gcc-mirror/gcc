// { dg-do compile { target c++11 } }
// PR c++/61663
// DR 976, strip ref from conv op return type before doing
// fn and ary decay or CV qual removal

struct F 
{
  template<class T>
  operator const T&();
};

void Foo () 
{
  F f;
  int i = f;
}

template<typename T> struct X {};

struct Y
{
  template<typename T> operator X<T> () &&; // #3
  template<typename T> operator X<T> const & () const &; // #4
};

void Use (X<void>);
Y Val ();
Y const &Ref ();

// { dg-final { scan-assembler "_Z5Frob3v:.*_ZNO1Ycv1XIT_EIvEEv.*_Z3Use1XIvE" } }
void Frob3 ()
{
  Use (Val ()); // #3
}

// { dg-final { scan-assembler "_Z5Frob4v:.*_ZNKR1YcvRK1XIT_EIvEEv.*_Z3Use1XIvE" } }
void Frob4 ()
{
  Use (Ref ()); // #4
}

struct Z 
{
  template<typename T> using FnRef = void (&) (T);
  template<typename T> using AryRef = T (&)[];

  template<typename T> operator FnRef<T> ();
  template<typename T> operator AryRef<T> ();
};

// { dg-final { scan-assembler "_Z5Frob5R1Z:.*_ZN1ZcvRFvT_EIiEEv.*_ZN1ZcvRA_T_IiEEv" } }
void Frob5 (Z &z)
{
  void (*fnptr)(int) = z;
  int *iptr = z;
}

// { dg-final { scan-assembler "_Z5Frob6R1Z:.*_ZN1ZcvRFvT_EIfEEv.*_ZN1ZcvRA_T_IfEEv" } }
void Frob6 (Z &z)
{
  void (&fnref)(float) = z;
  float (&aryref)[] = z;
}
