// { dg-do compile }
// { dg-options "-fgnu-tm" }

template<typename _Tp> class allocator
{
	public:
	allocator() { }
};
extern template class allocator<char>;

template<typename _Alloc = allocator<char> > class basic_string
{
	public:
	_Alloc _M_dataplus;

	__attribute__((transaction_safe))
	basic_string() : _M_dataplus(_Alloc())
	{
	}
};

int getStringHeight()
{
	basic_string<> tmp;
}
