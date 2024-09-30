// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }
// GROUPS passed conversions
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>

class cvec {
public:
        ~cvec(){ delete s; }
        cvec(const char*x) { s = new char[std::strlen(x)+1]; std::strcpy(s, x); }
	cvec(const cvec& c) { s = new char[std::strlen(c.s)+1]; std::strcpy(s, c.s); }
        operator const char*() { return s; }
private:
        char *s;
};

cvec
B(const char* a)
{
        return a;
}

void
A(const char* s)
{
        // s still ok here
        std::ifstream inf(s);
	if (std::strncmp ("aaa", s, 3))
	  {
	    std::printf ("FAIL\n");
	    std::exit (1);
	  }
	else
	  std::printf ("PASS\n");
}

int main()
{
        A(B("aaa"));
}
