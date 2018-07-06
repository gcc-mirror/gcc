// { dg-do compile }

extern "C" int abs (int);
struct a {
  short b;
} e;
short c;
bool
foo ()
{
  return abs(c) >= e.b;
}
