// g++ 1.37.1 bug 900520_05

// The following legal code gets syntax errors from g++.

// keywords: syntax, unimplemented, operator new, initialization, pointer types

struct struct_0 {
};

char *cp;
static struct_0 *sp;

void test0 ()
{
  new char * (cp);		// gets bogus error
}

void test1 ()
{
  new struct_0 * (sp);		// gets bogus error
}

int main () { return 0; }
