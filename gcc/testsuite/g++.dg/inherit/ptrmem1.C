// PR c++/14803
// { dg-options "-Werror" }

struct sc_module { int member; };

struct sc_signal_in_if { bool state; };

typedef void (sc_module::*SC_ENTRY_FUNC)();

class sc_clock : public sc_signal_in_if, public sc_module
{
public:
  sc_clock();
  void posedge_action();
  SC_ENTRY_FUNC fptr;
};

sc_clock::sc_clock()
{
  fptr = static_cast<SC_ENTRY_FUNC>(&sc_clock::posedge_action);
}
