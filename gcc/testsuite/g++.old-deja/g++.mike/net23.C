// { dg-do assemble  }

class environment {
public:
  int get_font() ;
};

typedef int (environment::*INT_FUNCP)();

void myfoo(INT_FUNCP a);

void init_env_requests()
{
  myfoo(&environment::get_font);
}
