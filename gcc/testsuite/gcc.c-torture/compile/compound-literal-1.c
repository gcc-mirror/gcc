/* ICE incrementing compound literal: bug 28418 from Volker Reichelt
   <reichelt@gcc.gnu.org>.  */

struct A { int i; };

void foo()
{
    ((struct A) { 0 }).i += 1;
}
