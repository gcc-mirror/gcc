// Origin: PR c++/43953

template<typename T,
         typename U,
	 typename T::type V> class bad;

// partial specialization
// for T = U
template<typename T, typename T::type V>
class bad<T, T, V>
{
public:
  static void foo() {}
};

struct dummy
{
  typedef int type;
};

int main()
{
  bad<dummy, dummy, 0>::foo();
}

