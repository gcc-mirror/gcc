// PR c++/85739

struct l { int k; };
template <int l::*> class b { };
template <const int l::*> class B { typedef int e; };
template <int l::*i, const int l::*n>
bool operator!=(B<n>, b<i>);

bool bb = (B<&l::k>() != b<&l::k>());

