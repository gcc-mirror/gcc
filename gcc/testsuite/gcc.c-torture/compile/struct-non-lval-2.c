/* Bug c/17855, using conditional expression for non-lvalue.  */
struct foo {char x, y, z[2];};
struct foo p, q; int r;
void bar(int baz)
{
  (r ? p : q).z[baz] = 1;
}
