// { dg-do assemble  }
// Bug: member operator shadows global template in tsubst.

class ostream;

template <class TP> class smanip;

template<class TP>
ostream& operator<<(ostream& o, const smanip<TP>& m);

template <class TP> class smanip {
public:
  friend ostream& operator<< <>(ostream &o, const smanip<TP>&m);
};

template<class TP>
ostream& operator<<(ostream& o, const smanip<TP>& m)
{ return o;}

class X
{
public:
  X operator<<(int);  // commenting out this line makes it work!
  void print(ostream& os);
};

void X::print(ostream& os)
{
  smanip<double> smd;
  os << smd;			// { dg-bogus "" } 
}
