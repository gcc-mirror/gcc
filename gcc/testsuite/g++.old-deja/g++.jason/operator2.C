// { dg-do assemble  }
// PRMS Id: 6018

class string {
    char *p;
public:
    string(const char* s) ;// { p == s; }
    operator const char*() ;// { return s; }
};
 
void f4(string& s)
{
        *s;     // implies "s.operator const char*()"
}
