// { dg-do compile }

// Origin: Matt Austern <austern@apple.com>

// PR c++/7983: ICE typedef to typename as friend.

template<class T> class smart_ptr2 {
    T* real_ptr;
 public:
    typedef typename T::subT  td;
    friend class td; // { dg-error "typedef" }
};
