// Build don't link:

class mystream;

template <class T> class a {
public:
	friend mystream& operator>> <>( mystream&, a<T>& thea );
private:
	T amember;
};

template <class T> mystream& operator>>( mystream& s, a<T>& thea );

template<> mystream& operator>> <int>( mystream& s, a<int>& thea );

template class a<int>;

template<> mystream& operator>> <int>( mystream& s, a<int>& thea )
{
	thea.amember = 0;
	return s;
}
