struct x {
    int foo () { return 0; }
};

template <class T>
struct vector {
    T& bar () { static T a; return a; }
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
