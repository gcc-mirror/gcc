struct x {
    int foo () {}
};

template <class T>
struct vector {
    T& bar () {}
};

template <class T>
struct y {
    typedef struct {   
        x t;
    } s;
    
    vector<s> array;

    int foo ()
      { return array.bar().t.foo(); }
};
int i = y<x>().foo ();
