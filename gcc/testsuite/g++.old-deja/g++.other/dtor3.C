// { dg-do assemble  }

struct S1
{
  ~S1(int); // { dg-error "" } destructors may not have parameters
};


template <class T>
struct S2
{
  ~S2(int); // { dg-error "" } destructors may not have parameters
};


struct S3 
{
  ~S3(double) {} // { dg-error "" } destructors may not have parameters
};


template <class T>
struct S4
{
  ~S4(double) {} // { dg-error "" } destructors may not have parameters
};


struct S5
{
  ~S5(); 
};

S5::~S5(float)  // { dg-error "" } destructors may not have parameters
{
}


template <class T>
struct S6
{
  ~S6();
};

template <class T>
S6<T>::~S6(float)   // { dg-error "" } destructors may not have parameters
{
}



