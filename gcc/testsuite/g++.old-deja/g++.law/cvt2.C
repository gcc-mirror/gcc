// GROUPS passed conversions
#include <stdio.h>
#include <stdlib.h>
#include <iostream.h>
#include <fstream.h>

extern "C" {
int strncmp (const char *, const char *, size_t);
}

class cvec {
public:
        ~cvec(){ delete s; }
        cvec(const char*x) { s = new char[strlen(x)+1]; strcpy(s, x); }
	cvec(const cvec& c) { s = new char[strlen(c.s)+1]; strcpy(s, c.s); }
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
        ifstream inf(s);
	if (strncmp ("aaa", s, 3))
	  printf ("FAIL\n");
	else
	  printf ("PASS\n");
}

int main()
{
        A(B("aaa"));
}

