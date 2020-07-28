/* PR preprocessor/96323 */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu99 -Wno-c++-compat -trigraphs" { target c } } */
/* { dg-options "-std=c++0x" { target c++ } } */
/* { dg-error "invalid new-line in raw string delimiter" "" { target *-*-* } .+2 } */
/* { dg-warning "missing terminating . character" "" { target *-*-* } .+2 } */
const char tu[] = R"a";
const char tua[] = "(R)a";
