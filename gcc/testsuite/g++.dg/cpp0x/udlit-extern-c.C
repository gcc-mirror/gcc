// { dg-do compile { target c++11 } }

extern "C" { // { dg-message "1: 'extern .C.' linkage started here" }

constexpr double operator"" _deg ( double degrees ); // { dg-error "literal operator with C linkage" }

}
