// { dg-do assemble  }
// GROUPS passed templates
template <class Q>
class Conc {
public:
	static int body();
};

template <class Q>
int Conc<Q>::body() {return 0;}

int main () {
	Conc<int> s2;
}
