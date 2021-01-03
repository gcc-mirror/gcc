// { dg-options "-fpermissive -w" }

template<int b> __attribute__ a([] { class c, __attribute__(vector_size(operator+()))) d; // { dg-error "before" }
// { dg-error "90:expected" "" { target *-*-* } .-1 }
