// { dg-do assemble  }

struct S1 {
  ~S1(); // { dg-error "" } candidate
};

S1::~S1() const
{ // { dg-error "" } prototype does not match 
}


struct S2 {
  ~S2() volatile; // { dg-error "" } destructors may not be volatile
};


template <class T>
struct S3 {
  ~S3(); // { dg-error "" } candidate
};

template <class T>
S3<T>::~S3() volatile
{ // { dg-error "" } prototype does not match 
}


template <class T>
struct S4 {
  ~S4() const; // { dg-error "" } destructors may not be const
};
