// { dg-do run }
namespace __gnu_cxx {
	template<bool> struct __pool {
		void _M_reclaim_block(char* p, unsigned long bytes);
	};
}

struct vector {
	~vector() { deallocate(0); }
	void deallocate(int* p) {
		if (p) {
			static __gnu_cxx::__pool<true> pool;
			pool._M_reclaim_block((char*)0, 0);
		}
	}
};

struct Foo { virtual void f() { } };

struct auto_ptr {
	Foo* ptr;
	auto_ptr() : ptr(0) { }
	~auto_ptr() { delete ptr; }
	Foo* release() { Foo* tmp = ptr; ptr = 0; return tmp; }
	void reset(Foo* p) { ptr = p; }
};

int main(int argc, char**) {
	auto_ptr foo;
	if (argc) {
		foo.reset(new Foo());
	} else {
		vector v;
	}
	Foo* p = foo.release();
	p->f();
}
