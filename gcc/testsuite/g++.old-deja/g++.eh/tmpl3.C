// { dg-do assemble  }

// Posted by Trevor Taylor <ttaylor@powerup.com.au>

template<class T> struct A {
    void X() throw(T);
};

template<class T>
inline void A<T>::X() 
throw(T) { } 
