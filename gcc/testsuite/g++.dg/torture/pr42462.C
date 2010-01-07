/* { dg-do run } */

#define INLINE inline __attribute__((always_inline))
extern "C" void abort (void);

template<class> struct Foo {
        inline bool isFalse() { return false; }
        template <bool>        void f1() {}
        template <bool> INLINE void f2() { f1<false>(); }
        template <bool>        void f3() { f2<false>(); }
        template <bool> INLINE void f4() { f3<false>(); }
        int exec2();
        void execute();
        inline void unused();
};

template<class T> inline void Foo<T>::unused() {
        f4<true>();
}

static int counter = 0;

template<class T> int Foo<T>::exec2() {
        static void* table[2] = { &&begin, &&end };	
	if (counter++ > 10)
	  return 0;
        goto *(table[0]);
begin:
        if (isFalse()) f1<false>();
end:
        return 1;
}

template<class T> void Foo<T>::execute() {
        int r = 1;
        while (r) { r = exec2(); }
}

template class Foo<int>;

int main() {
        Foo<int> c;
        c.execute();
	if (counter < 10)
	  abort ();
	return 0;
}
