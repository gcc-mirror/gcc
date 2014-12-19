// PR c++/61198
// { dg-do compile { target c++11 } }

template<int herp, typename derp_t>
struct broken
{
	template<typename target_t>
	using rebind = broken<herp, target_t>;
};

template<typename derp_t>
struct broken<2, derp_t>
{
	template<typename target_t>
	using rebind = broken<2, target_t>;
};

int main(int argc, char **argv)
{		
	broken<2, float>::rebind<double> u;

	return 0;
}

