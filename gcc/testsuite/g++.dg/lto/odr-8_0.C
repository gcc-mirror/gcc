// { dg-lto-do link }
struct a {char c;}; // { dg-lto-message "8: 'struct a' violates the C\\+\\+ One Definition Rule" }
int
test (struct a *a)
{
  return a->c;
}
