/* Bug c/17855.  */
struct foo {char x, y, z[2];};
struct foo f();
void bar(int baz)
{
  f().z[baz] = 1;
}
