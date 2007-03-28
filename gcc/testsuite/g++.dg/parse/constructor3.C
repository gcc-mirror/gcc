/* PR c++/29077 */
/* { dg-do "compile" } */

class c {
  c();
  c(const c&);
  ~c();
};

namespace m {
  c::c() {} /* { dg-error "c::c" } */
  c::c(const c&) {} /* { dg-error "c::c" } */
  c::~c() {} /* { dg-error "c::~c" } */
}
