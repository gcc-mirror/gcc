// Build don't link:

// Posted by Trevor Taylor <ttaylor@powerup.com.au>

template<class T> struct A {
    void X() throw(T); // gets bogus error - previous decl - XFAIL *-*-*
};

template<class T>
inline void A<T>::X() 
throw(T) { } // gets bogus error - different throws - XFAIL *-*-*
