// Build don't link:
// Special g++ Options: -ansi
// Origin: Theo Papadopoulo <Theodore.Papadopoulo@sophia.inria.fr>

inline const unsigned& f(unsigned const& a) {
        return a;
}

template <class T>
void
g(const unsigned n)
{
        double D[f(n)];
}

template <class T,class U>
void g(unsigned const int) { }

int main()
{
        g<double>(18);
}
