// PR c++/118528
// { dg-do compile { target c++20 } }
// { dg-options "" }

template<class T>
struct E { T t[130][2]; };

E e1 {
#embed __FILE__ limit (260)
};

template<class T>
struct F { T t[2][130]; };

F f1 {
#embed __FILE__ limit (260)
};
F f2 { { {
#embed __FILE__ limit (130)
}, {
#embed __FILE__ limit (130)
} } };
