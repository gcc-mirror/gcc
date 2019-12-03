// PR c++/44625
// { dg-do compile }
// { dg-options "" }
// { dg-additional-options "-Wno-return-type" }

template<typename FP_> struct Vec { // { dg-message "note" "" { target c++17_down } }
    Vec& operator^=(Vec& rhs)     {
        union {
            struct {FP_ x,y,z;}; // { dg-error "20:anonymous struct" }
        };
        X = y*rhs.z() - z*rhs.y(); // { dg-error "not declared|no member" }
    }
    Vec& operator^(Vec& rhs) {
        return Vec(*this)^=rhs; // { dg-message "required" }
    }
};
Vec<double> v(3,4,12); // { dg-error "no matching|too many initializers" }
Vec<double> V(12,4,3);  // { dg-error "no matching|too many initializers" }
Vec<double> c = v^V;   // { dg-message "required" }
