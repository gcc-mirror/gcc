// Special g++ Options: -O
// Build don't link:
// Bug: Synthesizing methods for the nested class screwed up current_class_decl
// for the outer class.

class A;
class AH
{
  public:
    AH (   A * p = 0 );
    AH ( const  AH & from )
    : pointer( from.pointer )   { inc(); }
    ~ AH ()  { dec(); }
  private:
    A * pointer;
    void inc() const;
    void dec() const;
};

class A 
{
  protected:
    struct AttrTable
    {
	struct Row
	{
	};
    };
    
  public:

    class Attributes
    {
      public:
	class iterator
	{
	  public:
	    iterator() : mo(0), attr(0) {}
	    iterator& operator++() { ++attr; return *this; }
	    iterator operator++(int)
	    { iterator tmp = *this; ++*this; return tmp; }

	  private:
	    AH mo;
	    const AttrTable::Row* attr;
	};

	Attributes(AH mo)
	: mo(mo) {}
	AH mo;
    };
};
