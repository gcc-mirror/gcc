// { dg-do compile { target c++11 } }

double
sqrt ();

namespace std {
  class gamma_distribution {
  public:
    gamma_distribution () : _M_param () {}

  private:
    struct param_type {
      param_type () : _M_beta () { _M_a2 = 1 / ::sqrt (); }
      double _M_beta, _M_a2;
    };
    param_type _M_param;
    int _M_saved_available, _M_saved = 0, _M_param0 = 0;
  };

  struct fisher_f_distribution {
    gamma_distribution _M_gd_x, _M_gd_y;
  };
}

int
main ()
{
  std::fisher_f_distribution d;

  return 0;
}
