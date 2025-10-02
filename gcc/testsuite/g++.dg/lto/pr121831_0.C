/* PR middle-end/121831 */
/* { dg-lto-do run } */
/* { dg-lto-options { { -O2 -flto } } } */

struct S { unsigned char s[256]; };
S a;
extern const S b[1];
struct T { unsigned char t[2048]; };
T c;
extern const T d[1];

__attribute__((noipa)) void
foo ()
{
  a = b[0];
  c = d[0];
}
