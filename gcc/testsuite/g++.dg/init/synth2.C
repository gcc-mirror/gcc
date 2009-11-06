// PR c++/34180

struct G {
  G();				// { dg-message "" "candidate" }
  G(G&);			// { dg-message "" "candidate" }
};

class A
{				// { dg-error "no match" }
  const G g;
};

int main()
{
  A a;
  A b = a;			// { dg-message "required here" }
}
