// Build don't link: 
// GROUPS passed old-abort
// Should have been fixed by:
//
// Sun Jun 13 12:55:22 1993  Brendan Kehoe  (brendan@lisa.cygnus.com)
// 
// 	* cp-cvt.c (build_default_binary_type_conversion): Look deeper into
// 	what ARG1 and ARG2 are if they're POINTER_TYPEs.

volatile void exit(int);

class CountableSet
{
	public:
		virtual	~CountableSet() { }		
};	

template<class T>
class FixedSet : virtual public CountableSet
{
	public:
		virtual	int Get(int, T&) = 0;		 
		virtual	~FixedSet() { }		
};

class ShrinkableSet
{
	public:
		virtual int Remove(int) = 0;   
};

template<class T>
class PVSet : virtual public FixedSet<T>, virtual public ShrinkableSet
{
	public:
		virtual	void Append(const T&) = 0;
		virtual	void operator+=(const T& a) { Append(a); }
		virtual	~PVSet() { }		
};

template<class T>
class MutSet : virtual public FixedSet<T>, virtual public FixedSet<T *>
{
	protected:
		typedef	T	*Tp;

	public:
		void Append(const Tp& tp) { Append(*tp); }

		T&	Access(int p)
		{
			Tp	tp;
			Get(p, tp);
			return *tp;
		}
		virtual	~MutSet() { }		
};

template <class T>
class	SimpleSet : virtual public MutSet<T>
{
	protected:
		T	*array;
		int	size;

		virtual	void	Allocate(int s)
		{
			array = new T[s];
		}
	public:
		SimpleSet()
		{
			size = 0;
			array = ((void*)0) ; // ERROR - implicit conversion
		}
 		int	Get(int p, T& t)
		{
			t = array[p-1];
			return 1;
		}
		int	Get(int p, T *& t)
		{
			t = &array[p-1];
			return 1;
		}
		inline void Append(const T& a)
		{
			array[size-1] = a;
		}
		inline int Remove(int n) { return 0; }
};

class	Dummy
{
	public:
		Dummy()	{}
};

int
main()
{
	SimpleSet<Dummy *>		bs1;
	int	i, j;
	Dummy	foo;

	bs1+=&foo;// ERROR -  no .*
}
