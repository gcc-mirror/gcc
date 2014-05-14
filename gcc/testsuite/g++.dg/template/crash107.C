// PR c++/44625
// { dg-do compile }
// { dg-options "" }

template<typename FP_> struct Vec { // { dg-message "note" }
    Vec& operator^=(Vec& rhs)     {
        union {
            struct {FP_ x,y,z;};
        }; // { dg-error "anonymous struct" }
        X = y*rhs.z() - z*rhs.y(); // { dg-error "not declared|no member" }
    }
    Vec& operator^(Vec& rhs) {
        return Vec(*this)^=rhs; // { dg-message "required" }
    }
};
Vec<double> v(3,4,12); // { dg-error "no matching" }
Vec<double> V(12,4,3);  // { dg-error "no matching" }
Vec<double> c = v^V;   // { dg-message "required" }
