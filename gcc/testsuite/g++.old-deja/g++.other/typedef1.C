// Build don't link:

typedef const struct {
   int x;
} Test;

void foo(Test);

void foo(Test t)
{
  t.x = 0; // ERROR - assignment of read-only member
  return;
}
