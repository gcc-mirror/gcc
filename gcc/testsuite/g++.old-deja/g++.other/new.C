// { dg-do assemble  }

#include <new>

inline void *
operator new(size_t alloc_sz, const char *fname, unsigned lineno)
{
  return ::operator new (alloc_sz);
}
inline void *
operator new[](size_t alloc_sz, const char *fname, unsigned lineno) 
{
  return ::operator new[] (alloc_sz);
}
inline void
operator delete(void *ptr, const char *fname, unsigned lineno) 
{
}
inline void
operator delete[](void *ptr, const char *fname, unsigned lineno)
{
}

class DEF {						 
public:
    DEF( DEF *parent=0, const char *name=0 );
};

class ABC
{
public:
    enum stuff { ID0, ID1 };
    ABC( stuff, DEF *parent=0, const char *name=0 );
};

class GHI : public DEF			 
{
};

class LMNFrame;
class LMN : public DEF
{
  friend class LMNFrame;
  public:
public:
  LMN();
private:
  LMNFrame *draw_area;
				 
  ABC *scroll_h;		 
};
class LMNFrame : public GHI {
};
LMN::LMN()
{
  draw_area = new ("abc", 69) LMNFrame;
				 
  scroll_h = new ("def", 71)  ABC(ABC::ID0, this);
}

