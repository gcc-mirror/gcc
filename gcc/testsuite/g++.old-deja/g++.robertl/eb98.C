// { dg-do assemble  }
// Error:    Internal compiler error in egcs 1998/05/28 snapshot.


    template<class T, unsigned int Length>
    inline
    unsigned int
    extent(T (&x)[Length])
    {
            return Length;
    }

    extern int b[];

    void f()
    {
      extent(b);  // { dg-error "" } no matching function
    }
