// Contribued by Gabriel Dos Reis <gdr@codesourcery.com>
// Origin: iskey@i100.ryd.student.liu.se

class ostream;
extern ostream& cout;

template <class T> struct s;

template <class T>
ostream& operator<<(ostream &o, const typename s<T>::t &x) // { dg-message "note" }
{
  return o;
}

template <class T>
struct s {
  struct t
  {
    friend ostream&
    operator<<<T>(ostream&, const typename s<T>::t &);
  };
  t x;
};

int main()
{
  s<int>::t y;
  cout << y; // { dg-error "" }
  // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } .-1 }
}
