/* { dg-do compile } */
/* { dg-options "-std=c++14 -O -fdump-tree-dse1-details" } */

using uint = unsigned int;

template<typename C, uint S>
struct FixBuf
{
	C buf[S] = {};
};

template<typename C>
struct OutBuf
{
	C*	cur;
	C*	end;
	C*	beg;

	template<uint S>
	constexpr
	OutBuf(FixBuf<C, S>& b) : cur{b.buf}, end{b.buf + S}, beg{b.buf} { }

	OutBuf(C* b, C* e) : cur{b}, end{e} { }
	OutBuf(C* b, uint s) : cur{b}, end{b + s} { }

	constexpr
	OutBuf& operator<<(C v)
	{
		if (cur < end) {
			*cur = v;
		}
		++cur;
		return *this;
	}

	constexpr
	OutBuf& operator<<(uint v)
	{
		uint q = v / 10U;
		uint r = v % 10U;
		if (q) {
			*this << q;
		}
		*this << static_cast<C>(r + '0');
		return *this;
	}
};

template<bool BOS>
struct BufOrSize
{
	template<typename C, uint S>
	static constexpr auto Select(FixBuf<C, S>& fb, OutBuf<C>&)
	{
		return fb;
	}
};

template<>
struct BufOrSize<true>
{
	template<typename C, uint S>
	static constexpr auto Select(FixBuf<C, S>&, OutBuf<C>& ob)
	{
		return ob.cur - ob.beg;
	}
};

// if BOS=1, it will return the size of the generated data, else the data itself
template<uint N, uint S, bool BOS = 0>
constexpr
auto fixbuf()
{
	FixBuf<char, S> fb;
	OutBuf<char> ob{fb};
	for (uint i = 0; i <= N; ++i) {
		ob << i << static_cast<char>(i == N ? 0 : ' ');
	}
	return BufOrSize<BOS>::Select(fb, ob);
}

auto foo()
{
	constexpr auto x = fixbuf<13, 200>();
	return x;
}

auto foo_sized()
{
	constexpr auto s = fixbuf<13, 0, 1>();
	constexpr auto x = fixbuf<13, s>();
	return x;
}

int main()
{
}


/* { dg-final { scan-tree-dump-times "MEM\\\[\\(struct FixBuf \\*\\)&<retval> \\+ \[0-9\]+B\\\] = {}" 1 "dse1" } } */

