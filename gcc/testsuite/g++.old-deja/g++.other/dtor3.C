// Build don't link:

struct S1
{
  ~S1(int); // ERROR - destructors may not have parameters
};


template <class T>
struct S2
{
  ~S2(int); // ERROR - destructors may not have parameters
};


struct S3 
{
  ~S3(double) {} // ERROR - destructors may not have parameters
};


template <class T>
struct S4
{
  ~S4(double) {} // ERROR - destructors may not have parameters
};


struct S5
{
  ~S5(); 
};

S5::~S5(float) 
{ // ERROR - destructors may not have parameters
}


template <class T>
struct S6
{
  ~S6();
};

template <class T>
S6<T>::~S6(float)
{ // ERROR - destructors may not have parameters
}



