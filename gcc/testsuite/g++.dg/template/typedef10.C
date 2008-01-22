// PR c++/34573

template < class Gtr_>
void compute_gr()
{
  typedef int Less_chain;
  struct utils {
    utils(const Less_chain& lc)  {};
  };
  utils U(1);
}
int main(void){
  compute_gr<int>();
}
