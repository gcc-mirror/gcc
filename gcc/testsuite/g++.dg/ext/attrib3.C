// Test that attributes work in a variety of situations.
// { dg-options -O }
// { dg-do run }

#define attrib __attribute ((mode (QI)))

attrib signed int a;		// attributes before type are broken
static attrib unsigned int b;

int foo(attrib int o)		// attribute arguments are broken
{
  return (sizeof (a) != 1
	  || sizeof (b) != 1
	  || sizeof (o) != 1
	  || sizeof ((attrib signed int) b) != 1);
}

int main ()
{
  return foo (42);
}
