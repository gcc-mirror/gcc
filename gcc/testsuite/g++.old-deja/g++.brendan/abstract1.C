// Build don't link: 
// GROUPS passed abstract-functions
class O
{
public:
   virtual int c()=0;
};

class I: public O
{
};

class S: public virtual I
{
public: 
  int c();
  virtual int v()=0;
};

class D: public S
{
   int v();
};

D *p=new D();
