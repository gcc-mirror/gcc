// Bug: member operator shadows global template in tsubst.
// Build don't link:

class ostream;

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
  os << smd;			// gets bogus error
}
