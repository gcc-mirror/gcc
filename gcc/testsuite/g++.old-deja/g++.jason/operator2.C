// PRMS Id: 6018
// Build don't link:

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
