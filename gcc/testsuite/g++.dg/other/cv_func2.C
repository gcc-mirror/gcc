typedef int ptr1() const; // no error

void foo ()
{
  typedef int ptr2() const; // no error
}

class C
{
    typedef int ptr3() const;  // error

    void bar ()
      {
        typedef int ptr4() const; // no error
      }  
};

void wibble () const { } // { dg-error "non-member function" }
