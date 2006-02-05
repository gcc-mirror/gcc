// { dg-do run  }
// PRMS Id: 4297 (related to 3525)
// Bug: Generating default op= didn't set TYPE_HAS_ASSIGNMENT, so it wasn't
// found.

extern "C" int printf (const char *, ...);

class Y 
{
public:
    Y(const char*) {}
    Y& operator = (const Y&) { return *this; }
};
    

class X
{
public:
    X(int v, const char* m) : _v(v), _m (m) {}
    X () : _v(0), _m("Unknown") {}
    // Defining our own op= here makes things work correctly.

private:
    int _v;
    int _m4;
    // Adding more members here increases the count on u.
    Y _m;
};

const X sPassed (1, "Passed"), sFailed (-1, "Failed");

int main (int, char**)
{
    X result;
    int u = 0;
    result = (u++ ? sPassed : sFailed);
    if (u == 1)
      return 0;
    return 1;
}
