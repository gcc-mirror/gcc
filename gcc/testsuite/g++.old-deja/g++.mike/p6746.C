// { dg-do assemble  }
// prms-id: 6746

class call_trace {
public:
  call_trace(char* fcn_name);
  ~call_trace();
};

static char * last_tree;
extern "C" void prt();

char * smt_mark_stree() {
  char* _my_name = "smt_mark_stree" ; 
  call_trace _t(_my_name);

  return last_tree = 0 ? (char*)0 : (prt(), (char*)0);
}
