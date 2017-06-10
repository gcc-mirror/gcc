// Origin: PR c++/38357
// { dg-do compile }

class BUG
{
public:
 bool name() { return true; }
};

template <bool T>
struct BUG1_5
{

};

template <bool name>
class BUG2 : BUG
{
public:
 typedef BUG1_5<name> ptr; // { dg-error "convert" }
};

int main()
{
 BUG2<false> b;
 return 0;
}
