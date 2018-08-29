/* { dg-do preprocess }*/
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++2a" { target c++ } } */

#define ERR1(x) __VA_OPT__ /* { dg-warning "__VA_OPT__ can only appear" } */
#define ERR2(x) __VA_OPT__( /* { dg-warning "can only appear" } */
#define ERR3(x) __VA_OPT__() /* { dg-warning "can only appear" } */

#define ERR4(x,...) __VA_OPT__ /* { dg-error "unterminated __VA_OPT__" } */
#define ERR5(x,...) __VA_OPT__( /* { dg-error "unterminated" } */
#define ERR6(x,...) __VA_OPT__(() /* { dg-error "unterminated" } */

#define ERR7(x,...) __VA_OPT__(__VA_OPT__) /* { dg-error "may not appear" } */
#define ERR7(x,...) __VA_OPT__(__VA_OPT__()) /* { dg-error "may not appear" } */

#define ERR8(x, y,...) x __VA_OPT__(##) y /* { dg-error "either end" } */
#define ERR9(x, y,...) x __VA_OPT__(x ##) y /* { dg-error "either end" } */
#define ERRA(x, y,...) x x __VA_OPT__(## y) /* { dg-error "either end" } */

#define ERRB __VA_OPT__ /* { dg-warning "can only appear" } */
#define ERRC(__VA_OPT__) x /* { dg-warning "can only appear" } */

__VA_OPT__ /* { dg-warning "can only appear" } */

#define ERRD(x)
ERRD(__VA_OPT__) /* { dg-warning "can only appear" } */

#define __VA_OPT__ /* { dg-warning "can only appear" } */
