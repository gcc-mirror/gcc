// { dg-do assemble  }
// PRMS Id: 4257
// Bug: g++ ignores non-member possibilities (ideal_candidate_ansi bug)

class ostream 
{
public:
#ifdef EITHER_ONE_A
    ostream& operator<<(unsigned long n);
    ostream& operator<<(long n);
#else
    ostream& operator<<(short n);
    ostream& operator<<(unsigned short n);
#endif
};

class ccObjectInfo {};

ostream& operator << (ostream& out, const ccObjectInfo& obj);

class ccString : public ccObjectInfo
{
#ifdef EITHER_ONE_B
    operator int		() const;
#else
    operator long   		() const;
#endif
};

// Should pick this one!!
ostream& operator << (ostream& o, const ccString & s);

extern ostream cout;

void f ()
{
    ccString foo;
    cout << foo;
}
