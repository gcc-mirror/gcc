// PR c++/18416

class errarg { 
  enum { EMPTY } type; 
public: 
  errarg(); 
}; 
extern errarg empty_errarg; 
extern void errprint(const char *, 
		     const errarg &arg1 = empty_errarg, 
		     const errarg &arg2 = empty_errarg, 
		     const errarg &arg3 = empty_errarg); 
errarg::errarg() : type(EMPTY) 
{ 
} 
errarg empty_errarg; 
