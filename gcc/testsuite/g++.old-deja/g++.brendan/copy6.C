// { dg-do run  }
// GROUPS passed copy-ctors
/*
g++ 2.3.3 will prefer using type conversions over the
implicitly generated copy constructor. This is wrong.
If you explicitly define a copy constructor, it will
use it. However, the implicit copy constructor MUST be
called whenever an explicit one would have been called
also. See below: g++ converts from and back into
unsigned, instead of using the implicit copy constructor:
here is the version:
Reading specs from /usr/lib/gcc-lib/i386-linux/2.3.3/specs
gcc version 2.3.3
 /usr/lib/gcc-lib/i386-linux/2.3.3/cpp -lang-c++ -v -undef -D__GNUC__=2 -D__GNUG__=2 -D__cplusplus -Dunix -Di386 -Dlinux -D__unix__ -D__i386__ -D__linux__ -D__unix -D__i386 -D__linux bug2.cc /usr/tmp/cca02008.i
GNU CPP version 2.3.3 (80386, BSD syntax)
 /usr/lib/gcc-lib/i386-linux/2.3.3/cc1plus /usr/tmp/cca02008.i -quiet -dumpbase bug2.cc -version -o /usr/tmp/cca02008.s
GNU C++ version 2.3.3 (80386, BSD syntax) compiled by GNU C version 2.3.3.
 as -o /usr/tmp/cca020081.o /usr/tmp/cca02008.s
 ld /usr/lib/crt0.o -nojump -L/usr/lib/gcc-lib/i386-linux/2.3.3 /usr/tmp/cca020081.o -lg++ -lgcc -lc -lgcc

Ok, and here is the output:
test k: constructing from scratch
test l=k: type conversion into unsigned
constructing from unsigned

*/

extern "C" int printf (const char *, ...);
extern "C" void exit (int);

int count = 0;

void die () { printf ("FAIL\n"); exit (1); }

struct test {
	test() { if (count != 0) die (); }

	test(unsigned) {
	  die ();
	}
	operator unsigned() {
	  die ();
	  return 0;
	}
};

int
main() {
  test k;
  test l=k;

  printf ("PASS\n");

  return 0;
}

