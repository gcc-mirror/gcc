// PR c++/93901
// { dg-do compile { target c++11 } }

void *operator new (__SIZE_TYPE__, void *p) noexcept { return p; }

extern void *mem;

constexpr bool YES = true;

struct NoexceptTrueCtor {
	NoexceptTrueCtor() noexcept(true);
};
void NoexceptTrueFun() noexcept(true);

struct NoexceptYesCtor {
	NoexceptYesCtor() noexcept(YES);
};
void NoexceptYesFun() noexcept(YES);

struct NoexceptOneEqOneCtor {
	NoexceptOneEqOneCtor() noexcept(1 == 1);
};
void NoexceptOneEqOneFun() noexcept(1 == 1);

struct NoNoexceptCtor {
	NoNoexceptCtor();
};
void NoNoexceptFun();

static_assert(noexcept(new(mem) NoexceptTrueCtor), "2"); // OK
static_assert(noexcept(NoexceptTrueFun()), "3"); // OK

static_assert(noexcept(new(mem) NoexceptYesCtor), "5"); // fail
static_assert(noexcept(NoexceptYesFun()), "6"); // OK

static_assert(noexcept(new(mem) NoexceptOneEqOneCtor), "8"); // fail
static_assert(noexcept(NoexceptOneEqOneFun()), "9"); // OK

static_assert(!noexcept(new(mem) NoNoexceptCtor), "11"); // OK
static_assert(!noexcept(NoNoexceptFun()), "12"); // OK
