/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic" { target c } } */
/* { dg-options "-std=c++11 -pedantic" { target c++ } } */

¨ /* { dg-error "is not valid in an identifier" "" { target c++ } } */

B̀

̀ /* { dg-error "not valid at the start of an identifier" } */

À /* { dg-warning "not in NFC" } */

𐀀
🿽	/* { dg-error "is not valid in an identifier" "" { target c++ } } */
󡈴	/* { dg-error "is not valid in an identifier" "" { target c++ } } */
