/* Bug c/17855, using assignment for non-lvalue.  */
struct foo {char x, y, z[2];};
struct foo p, q;
void bar(int baz)
{
  (p = q).z[baz] = 1;
}
