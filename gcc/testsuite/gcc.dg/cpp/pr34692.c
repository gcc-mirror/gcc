/* PR preprocessor/34692 */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "vara" } } */
/* { dg-final { scan-hidden "varb" } } */
/* { dg-final { scan-hidden "varc" } } */
/* { dg-final { scan-hidden "vard" } } */
/* { dg-final { scan-assembler "a b cde f g h" } } */

#define FOO(y, x) y #x
#define BAR(x) x
#define BAZ(x) x
FOO (const char *vara =,
a
#pragma GCC visibility push(hidden)
b
#pragma GCC visibility push(hidden)
cde f g h);
int varb = 6;
#pragma GCC visibility pop
#pragma GCC visibility pop
FOO (
BAR (
#pragma GCC visibility push(hidden)
const) char *varc =,);
#pragma GCC visibility pop
FOO (
BAR (
BAZ (
#pragma GCC visibility push(hidden)
#pragma GCC visibility push(hidden)
const) char) *vard =,);
#pragma GCC visibility pop
#pragma GCC visibility pop
