// Build don't link:
// GROUPS passed templates membertemplates
template<class P_numtype, int N_length>
class TinyVector {};

template<class P_numtype, int N_rank>
struct Array 
{
    template<int N_rank2>
    Array() {}
  
    template<int N_rank2>
    P_numtype operator()(const TinyVector<int,N_rank2>& index) const {}
};

