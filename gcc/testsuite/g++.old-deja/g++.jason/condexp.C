// { dg-do assemble  }
// PRMS id: 5629

struct String { const char *x; };
class Pathname: public String { };

String
f(int i)
{
    Pathname p;
    String s;

    return i ? p: s;
}
