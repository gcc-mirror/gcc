// PR c++/82664

template < typename > struct target_disambiguator;
template < typename R, typename A1 > struct target_disambiguator< R(A1) > {
  typedef A1 type;
  template < R (&)() > struct layout;
};

int main() {
  typedef target_disambiguator< void (int) > ::type target_type ;
}
