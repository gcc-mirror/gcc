// { dg-do assemble  }

struct S1 { // { dg-message "defined here" }
  ~S1(); // { dg-message "candidate" }
};

S1::~S1() const // { dg-error "no declaration matches" }
{
}


struct S2 {
  ~S2() volatile; // { dg-error "" } destructors may not be volatile
};


template <class T>
struct S3 { // { dg-message "defined here" }
  ~S3(); // { dg-message "candidate" }
};

template <class T>
S3<T>::~S3() volatile  // { dg-error "no declaration matches" }
{
}


template <class T>
struct S4 {
  ~S4() const; // { dg-error "" } destructors may not be const
};
