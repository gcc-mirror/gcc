// { dg-do run  }
// prms-id: 658

#include <iostream>
#include <cstdlib>

/* We may not find the libg++ <bool.h>.  */
#ifndef FALSE
#define FALSE false
#endif
#ifndef TRUE
#define TRUE true
#endif

// The VxWorks kernel-mode headers define a macro named "OK", which is not
// ISO-compliant, but is part of the VxWorks API.
#if defined __vxworks && !defined __RTP__
#undef OK
#endif

class Object {
public:
    Object();
    Object(const Object&);
    ~Object();

    void OK() const;
private:
    bool _destructed;
};

class Char: public Object {
public:
    Char();
    Char(char);
    Char(const Char&);
    ~Char();

    operator char () const;
private:
    char _c;
};

int main()
{
    Char r, s;

    r = Char('r');
    s = Char('s');
}

//
// Object stuff
//
Object::Object():
_destructed(FALSE)
{}

Object::Object(const Object& other):
_destructed(FALSE)
{
    other.OK();
}

Object::~Object()
{
    OK();
    _destructed = TRUE;
}

void
Object::OK() const
{
    if (_destructed) {
	std::cerr << "FAILURE - reference was made to a destructed object\n";
	std::abort();
    }
}

//
// Char stuff
//

Char::Char():
Object(),
_c('a')
{ }

Char::Char(char c):
Object(),
_c(c)
{ }

Char::Char(const Char& other):
Object(other),
_c(other._c)
{ }

Char::~Char()
{
    OK();
}

Char::operator char () const
{
    return _c;
}


