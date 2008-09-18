// { dg-options "-fshow-column" }
struct INCOMPLETE;
template <int> struct X {
    static INCOMPLETE value;
};
template <> INCOMPLETE X<1>::value = 0; // { dg-error "30:variable 'INCOMPLETE X<1>::value' has initializer but incomplete type" }

