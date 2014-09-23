// PR c++/61745

template <typename INT,INT P> class Zp;

template <typename INT,INT P> 
Zp<INT,P> operator-(const Zp<INT,P>& a, const Zp<INT,P>& b);

template <typename INT,INT P>
class Zp {
public:
  static const INT p = P;
private:
  INT val;
public:
  Zp() : val(0) {}
  Zp( INT x ) : val(x%p) { if (x < 0 ) x+= p; }

  // this compiles only if the following definition is moved
  // AFTER the friend declaration
  Zp  operator-() const { return Zp(p-val); }
  friend Zp<INT,P> operator- <>(const Zp<INT,P>& a, const Zp<INT,P>& b); // { dg-error "declaration|expected" }
};
