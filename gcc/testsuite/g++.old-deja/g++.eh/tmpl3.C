// { dg-do assemble  }

// Posted by Trevor Taylor <ttaylor@powerup.com.au>

template<class T> struct A {
    void X()
#if __cplusplus <= 201402L
    throw(T)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
    ;
};

template<class T>
inline void A<T>::X() 
#if __cplusplus <= 201402L
throw(T)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{ } 
