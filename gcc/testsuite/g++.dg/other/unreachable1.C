// PR middle-end/17827
// Origin: Andre Woebbeking <Woebbeking@web.de>
// Testcase by Volker Reichelt <reichelt@gcc.gnu.org>
// { dg-do compile }

void foo()
{
  if (false)
    if (int i=0)
      int j=i;
}
