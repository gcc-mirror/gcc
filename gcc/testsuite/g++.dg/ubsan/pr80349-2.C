// PR sanitizer/80349
// { dg-do compile }
// { dg-options "-fsanitize=undefined" }

unsigned long int ll;

int
foo ()
{
  return (2036854775807 >> ll & char(207648476159223) | 502810590243120797UL) << 0;
}
