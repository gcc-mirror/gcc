// { dg-do assemble  }
// GROUPS passed gb scope
template<class T, int N> class FixedArray_t {
public:
  T _array[N];

  FixedArray_t () { }
};

typedef FixedArray_t<unsigned char, 4> IPAddress_t;

class IPAddress {
public:
  IPAddress (IPAddress_t ip) { }

  operator IPAddress_t ()
  {
    IPAddress_t rv;
    return rv;
  }

  IPAddress () { }
};

class DatagramHeader {
public:
  IPAddress _src;

  void setHeader (IPAddress);
};

void
DatagramHeader::setHeader (IPAddress)
{
}
