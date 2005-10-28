// { dg-do compile }

// Origin: stefaandr@hotmail.com

// PR c++/12403: ICE when explicit specialization is not in
// namespace scope.

struct foo { 
        template<typename T> void bar (T &t) {}
        template<> void bar<double>(double &t) {} // { dg-error "non-namespace|template|function" }
};
