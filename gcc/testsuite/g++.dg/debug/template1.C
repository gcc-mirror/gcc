// PR c++/5118

template <int Count>
class d
{
public:
  d() 
  { 
        myInt = Count;
  }
  int myInt;
  virtual ~d() {}
};

volatile d<5> instD;
