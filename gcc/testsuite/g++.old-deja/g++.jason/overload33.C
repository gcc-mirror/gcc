// { dg-do assemble  }
// PRMS ID: 7507

/* ------------------------------------------------------------ */

class Base0
{
public:
				Base0() {}
      virtual			~Base0() {}
};

class Base1
{
public:
				Base1() {}
    virtual			~Base1() {}
};

class Derived : public Base0, public Base1
{
public:
				Derived() {}
  virtual			~Derived() {}
};

/* ------------------------------------------------------------ */

class Dummy
{
  public:
				Dummy(Base0 * theBase) {}
				~Dummy() {}
};

/* ------------------------------------------------------------ */

template<class T>
class ConstSmartPtr
{
  T*			myItem;		// private

  public:	
			ConstSmartPtr(T const* theItem);

			operator T const*() const
				{ return myItem; }
  protected:
    T*			_item() const
				{ return myItem; }
};

template<class T>
class SmartPtr : public ConstSmartPtr<T>
{
  public:
			SmartPtr(T* theItem)
			  : ConstSmartPtr<T>(theItem) {}

    T*			item() const
				{ return this->_item(); }

			operator T*() const
				{ return this->_item(); }
};

/* ------------------------------------------------------------ */

void
function()
{
  SmartPtr<Derived>  myObj = new Derived();

  Dummy th1(myObj);		    // Doesn't work under Cygnus
  Dummy th2((Base0 *) myObj);	    // Doesn't work either
}

/* ------------------------------------------------------------ */
