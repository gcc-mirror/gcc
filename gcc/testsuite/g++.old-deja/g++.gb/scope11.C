// Build don't link: 
// GROUPS passed gb scope
template<class T, int N> class fixed_array {
public:
  T _array[N];
};

class Hash {
public:
  Hash (int);
};

typedef fixed_array<char, 4> ipAddress_t;

class IPAddress {
protected:
  long _i;
public:
  IPAddress (ipAddress_t ip) { }
  IPAddress () { }
  IPAddress netMask () { return *this; }
  operator Hash ();
};

IPAddress::operator Hash ()
{
  return Hash (_i);
}
