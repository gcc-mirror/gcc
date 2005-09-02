// PR c++/23167

struct dom
{
  static int tostr();
  void eval_old() volatile{tostr();}
  ~dom() throw();
};

