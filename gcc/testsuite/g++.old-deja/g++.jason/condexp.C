// PRMS id: 5629
// Build don't link:

struct String { const char *x; };
class Pathname: public String { };

String
f(int i)
{
    Pathname p;
    String s;

    return i ? p: s;
}
