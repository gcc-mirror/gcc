// PR c++/6392
// Bug: We tried to make the array restricted, which doesn't make sense.

template <class T>
class bob
{	
	T * __restrict a[50];
};
