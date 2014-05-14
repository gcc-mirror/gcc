// { dg-do assemble  }
// Ambiguous conversion, three candidates:
// builtin == (int, int), and the two user-defined operators
// Each one requires a user-defined ICS where another uses builtin conversions,
// so none is the best viable function.

class MyInt
{
public:
        MyInt(int = 0) {}
        operator int() const {return 2;}
};

bool operator==(const MyInt& a, const int& b)   // { dg-message "operator==" } candidate
{
        return (int)a == b;
}

bool operator==(const MyInt& a, const MyInt& b) // { dg-message "operator==" } candidate
{
        return (int)a == (int)b;
}

bool f()
{
  return 3 == MyInt();                          // { dg-error "ambiguous" "err" } 
  // { dg-message "operator==" "match candidate text" { target *-*-* } 26 }
}
