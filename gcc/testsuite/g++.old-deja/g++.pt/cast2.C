// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Jan 2001 <nathan@codesourcery.com>

// Bug 1588. We ICE'd on reparsing an absdcl as a cast inside a template
// function.

class A {
public:
 template <class T> void f(void *CLUTp);
};

template <class T> void A::f(void *CLUTp)
{
    void *CLUT;

    CLUT = (unsigned char [3][256])CLUTp; // { dg-error "" } cast to array

    return;
}


int main()
{
	A myobj;
	unsigned char t[3][256];

	myobj.f<unsigned char>(t);
		
	return 0;
}
