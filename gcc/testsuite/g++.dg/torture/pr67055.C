// { dg-do compile }
// { dg-additional-options "-std=c++14" }

namespace std {
    typedef __SIZE_TYPE__ size_t;
    struct nothrow_t;
}
namespace vespamalloc {
    void fn1(void *);
    template <typename> class A {
    public:
	static unsigned long fillStack(unsigned long);
    };
    template <typename StackRep>
	unsigned long A<StackRep>::fillStack(unsigned long p1) {
	    void *retAddr[p1];
	    fn1(retAddr);
	    return 0;
	}
    class B {
    protected:
	B(void *);
    };
    template <int StackTraceLen> class D : B {
    public:
	D() : B(0) {}
	void alloc(int) { A<int>::fillStack(StackTraceLen); }
    };
    template <typename, typename> class C {
    public:
	void *malloc(unsigned long);
    };
    template <typename MemBlockPtrT, typename ThreadListT>
	void *C<MemBlockPtrT, ThreadListT>::malloc(unsigned long) {
	    MemBlockPtrT mem;
	    mem.alloc(0);
	    return 0;
	}
    C<D<16>, int> *_GmemP;
}
void *operator new(std::size_t, std::nothrow_t &) noexcept {
    return vespamalloc::_GmemP->malloc(0);
}
void *operator new[](std::size_t, std::nothrow_t &) noexcept {
    return vespamalloc::_GmemP->malloc(0);
}
