// PR c++/39425
// { dg-do compile }

class a {

  template<unsigned int s>
    struct _rec {
    static const char size = _rec< (s >> 1) >::size; // { dg-error "depth" }
    };

  template<>	// { dg-error "explicit" }
  struct _rec <0> { // { dg-error "too few" }
    static const char size = 0;
  };

  static const unsigned int value = _rec < 1 >::size;

};

// { dg-prune-output "compilation terminated" }
