// { dg-do assemble }
// { dg-xfail-if "" { sparc64-*-elf } }
// { dg-options "-pedantic-errors -g -Wall" }
// GROUPS passed synthetics
// Check to make sure that g++ doesn't get freaked out about the use
// of generated methods that it supposedly "can't see".

class Char {
public:
    Char();
    Char(char v);
    
    // synthetic copy-constructor and operator=
private:
    char value;
};

class XChar: public Char {
public:
    XChar();
    XChar(char v, int serial);
    
    void operator=(const XChar& other);
private:
    int serial;
};

void
XChar::operator=(const XChar& other)
{
    Char::operator=(other);
    // serial stays the same
}
