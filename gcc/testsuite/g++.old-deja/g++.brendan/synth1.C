// Build don't link: 
// Special g++ Options: -pedantic-errors -g -Wall
// GROUPS passed synthetics
// excess errors test - XFAIL sparc64-*-elf
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
