// Build don't link:

struct S1 {
  ~S1(); // ERROR - candidate
};

S1::~S1() const
{ // ERROR - prototype does not match 
}


struct S2 {
  ~S2() volatile; // ERROR - destructors may not be volatile
};


template <class T>
struct S3 {
  ~S3(); // ERROR - candidate
};

template <class T>
S3<T>::~S3() volatile
{ // ERROR - prototype does not match 
}


template <class T>
struct S4 {
  ~S4() const; // ERROR - destructors may not be const
};
