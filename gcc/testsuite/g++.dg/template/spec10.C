// { dg-do run }

// Origin: Lynn Akers <lakers@peachtree.com>

// PR c++/10940: Problem handling parameter list for static member
// that is a specialization of a member template of a template class.

template<int b>
class o
{
public:
	template<typename T> static void do_add(T* p, T v);
};

template<>
template<typename T>
inline void o<32>::do_add(T* p, T v)
{
	*p += v;
}

int main()
{
	int a = 0x1000;
	o<32>().do_add<int>(&a, 0x2000);
	return a;
}
