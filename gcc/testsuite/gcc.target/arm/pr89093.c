/* PR target/89093 */
/* { dg-do compile } */

__attribute__((target ("arm.foobar"))) void f1 (void) {} /* { dg-error "unknown target attribute or pragma 'arm.foobar'" } */
__attribute__((target ("thumbozoo1"))) void f2 (void) {} /* { dg-error "unknown target attribute or pragma 'thumbozoo1'" } */
__attribute__((target ("arm,thumbique"))) void f3 (void) {} /* { dg-error "unknown target attribute or pragma 'thumbique'" } */
__attribute__((target ("thumb981,arm"))) void f4 (void) {} /* { dg-error "unknown target attribute or pragma 'thumb981'" } */
