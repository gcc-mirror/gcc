// Build don't link: 
// GROUPS passed error-messages
class X {
public:
    static int x;// ERROR -  previous.*
    static int y;// ERROR -  previous.*
};

unsigned X::x;// ERROR -  conflict.*
unsigned X::y;// ERROR -  conflict.*
