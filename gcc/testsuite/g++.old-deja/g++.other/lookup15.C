// Build don't link:
// Origin: Benjamin Kosnik <bkoz@cygnus.com>

class b
{
  int j;
public:
  b(int a = 6): j(a) {}
  void imbue(int a) {++j;}
};

class d: public b
{
  int k;
public:
  d(int a = 7): b(a), k(a) {}
  void imbue(int a) {++k;}
};
  
//virtual public kills, public ok
class mostd: virtual public d
{
  int l;
public:
  mostd(int a = 9): d(a), l(a) {}
};

int main() {

  d dobj;
  dobj.imbue(5);

  mostd mobj;
  mobj.imbue(5);
  
  return 0;
}
