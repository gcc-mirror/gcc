// Test that attributes work in a variety of situations.
// { dg-options "-O -ftrack-macro-expansion=0" }
// { dg-do run { target c++11 } }

#define attrib [[gnu::mode (QI)]]
#define gnu_attrib __attribute((mode (QI)))

attrib signed int a;
static unsigned int b attrib;

int foo(attrib int o)
{
  return (sizeof (a) != 1
	  || sizeof (b) != 1
	  || sizeof (o) != 1
	  || sizeof ((gnu_attrib signed int) b) != 1);
}

int main ()
{
  return foo (42);
}
