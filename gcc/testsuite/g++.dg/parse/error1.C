struct INCOMPLETE;
template <int> struct X {
    static INCOMPLETE value;
};
template <> INCOMPLETE X<1>::value = 0; // { dg-error "" }

