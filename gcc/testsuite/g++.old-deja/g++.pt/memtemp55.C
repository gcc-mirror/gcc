// Build don't link:
// GROUPS passed templates membertemplates
template<class X> class  _bz_update { };

template<class T>
struct S {
template<int N_destRank>
void foo() { _bz_update<int>(); }
};

