/* { dg-do compile } */
/* { dg-require-ifunc "" } */

class b
{
public:
  __attribute__ ((target ("aes"))) b () {}
  __attribute__ ((target ("default"))) b () {}
};
class c
{
  b d;
};
void
fn1 ()
{
  c a;
}
__attribute__ ((target_clones ("sse", "default"))) void
e ()
{
}
