// Build don't link:
// Origin: Ian Nixon <ian@tharas.com>

class Action {
public:
  virtual void action () = 0;
};

class Var {
public:

  template<class Base> void Add() {
	struct tmp : public Action {
	  void action () {}
	};
	tmp *tp = new tmp; 
  }

};
