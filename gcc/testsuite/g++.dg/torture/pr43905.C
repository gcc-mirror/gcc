extern void sf ( __const char *);
struct Matrix{
  int operator[](int n){
    sf ( __PRETTY_FUNCTION__);
  }
  int operator[](int n)const{
    sf ( __PRETTY_FUNCTION__);
  }
};
void calcmy(Matrix const &b, Matrix &c, int k){
  b[k];
  c[k];
}
