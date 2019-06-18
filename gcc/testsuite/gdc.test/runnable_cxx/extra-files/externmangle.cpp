#include <stddef.h>
#include <stdint.h>

template<class X>
struct Foo
{
    X *v;
};

template<class X>
struct Boo
{
    X *v;
};

void test1(Foo<int> arg1)
{
}


void test2(int* arg2, Boo<int*> arg1)
{
}

template<int X, int Y>
struct Test3
{

};

void test3(Test3<3,3> arg1)
{
}

void test4(Foo<int*> arg1, Boo<int*> arg2, Boo<int*> arg3, int*, Foo<double>)
{
}

void test5(Foo<int*> arg1, Boo<int*> arg2, Boo<int*> arg3)
{
}

struct Goo
{

    template<class X>
    struct Foo
    {
        X* v;
    };

    template<class X>
    struct Boo
    {
        template<class Y>
        struct Xoo
        {
            Y* v;
        };
        X* v;
    };


    void test6(Foo<Boo<Foo<void> > > arg1);
    void test7(Boo<void>::Xoo<int> arg1);
};

void Goo::test6(Goo::Foo<Goo::Boo<Goo::Foo<void> > > arg1)
{
}

void Goo::test7(Goo::Boo<void>::Xoo<int> arg1)
{
}

struct P1
{
    template<class T>
    struct Mem
    {
    };
};

struct P2
{
    template<class T>
    struct Mem
    {
    };
};

void test8(P1::Mem<int>, P2::Mem<int>){}
void test9(Foo<int**>, Foo<int*>, int**, int*){}



class Test10
{
    private: void test10();
    public: void test11();
    protected: void test12();
    public: void test13() const;

    private: void test14(); // Private methods in D are always non-virtual
    public: virtual void test15();
    protected: virtual void test16();

    private: static void test17();
    public: static void test18();
    protected: static void test19();
};

Test10* Test10Ctor()
{
    return new Test10();
}

void Test10Dtor(Test10*& ptr)
{
    delete ptr;
    ptr = 0;
}

void Test10::test10(){}
void Test10::test11(){}
void Test10::test12(){}
void Test10::test13() const{}
void Test10::test14(){}
void Test10::test15(){}
void Test10::test16(){}
void Test10::test17(){}
void Test10::test18(){}
void Test10::test19(){}

struct Test20
{
    private: static int test20;
    protected: static int test21;
    public: static int test22;
};

int Test20::test20 = 20;
int Test20::test21 = 21;
int Test20::test22 = 22;

int test23(Test10**, Test10*, Test10***, Test10 const *const)
{
    return 1;
}

int test23b(Test10 const *const *const,  Test10 const* const, Test10*)
{
    return 1;
}

void test24(int(*)(int,int))
{
}

void test25(int arr[2][5][6][291])
{
}

int test26(int arr[5][6][291])
{
    return arr[1][1][1];
}

void test27(int, ...){}
void test28(int){}

void test29(float){}
void test30(const float){}

template<class T>
struct Array
{
    int dim;
};

class Module
{
public:
    static void imports(Module*);
    static int dim(Array<Module*>*);
};


void Module::imports(Module*)
{
}

int Module::dim(Array<Module*>* arr)
{
    return arr->dim;
}

uint64_t testlongmangle(int a, unsigned int b, int64_t c, uint64_t d)
{
    return a + b + c + d;
}

unsigned long testCppLongMangle(long a, unsigned long b)
{
    return a + b;
}

unsigned long long testCppLongLongMangle(long long a, unsigned long long b)
{
    return a + b;
}

size_t testCppSizeTMangle(ptrdiff_t a, size_t b)
{
    return a + b;
}

int test31[2][2][2] = {1, 1, 1, 1, 1, 1, 1, 1};
int *test32 = 0;



class Expression;

typedef int (*apply_fp_t)(Expression*, void*);

class Expression
{
    int type;
public:
    int apply(apply_fp_t fp, apply_fp_t fp2, void *param);
    int getType();
    static Expression* create(int v);
    static void dispose(Expression*&);
};

int Expression::apply(apply_fp_t fp, apply_fp_t fp2, void *param)
{
    return fp(this, param) * fp2(this, param);
}

int Expression::getType()
{
    return type;
}

Expression* Expression::create(int v)
{
    Expression *e = new Expression();
    e->type = v;
    return e;
}

void Expression::dispose(Expression *&e)
{
    if (e)
        delete e;
    e = 0;
}

/*int test34(int v[0][0][0])
{
    return 0;
}*/

#ifndef _MSC_VER
    int test35(long double arg)
    {
        return (int)arg;
    }
#endif

const char *test36(const char *arg)
{
    return arg;
}

class Test37
{
public:
    static Test37 *create();
    bool test();
};

bool test37()
{
    Test37 *o = Test37::create();
    return o->test();
}

class Test38
{
public:
     int test(int, ...);
     static Test38* create();
     static void dispose(Test38*&);
};

int Test38::test(int a, ...)
{
    return a;
}

Test38* Test38::create()
{
    Test38 *t = new Test38();
    return t;
}

void Test38::dispose(Test38 *&t)
{
    if (t)
        delete t;
    t = 0;
}

class S1
{
    int val;
public:
    static S1* init(int);
    S1(int v) : val(v) {}
    int value();
};

S1* S1::init(int x)
{
    return new S1(x);
}

int S1::value()
{
    return val;
}

template<class T>
class S2
{
    T val;
public:
    static S2<T>* init(T);
    S2(T v) : val(v) {}
    T value();
};

template<>
S2<int>* S2<int>::init(int x)
{
    return new S2<int>(x);
}

template<>
int S2<int>::value()
{
    return val;
}

struct C1
{
    const char *data;

    static C1* init(const char *p);

    C1(const char* p) : data(p) { }

    virtual const char* getDataCPP();
    virtual const char* getDataD();
};

C1* C1::init(const char *p)
{
    return new C1(p);
}

const char* C1::getDataCPP()
{
    return data;
}

template<class T>
struct C2
{
    const T *data;

    static C2* init(const T *p);

    C2(const T* p) : data(p) { }

    virtual const T* getData();
};

template<>
C2<char>* C2<char>::init(const char *p)
{
    return new C2<char>(p);
}

template<>
const char* C2<char>::getData()
{
    return data;
}

int test39cpp(C2<char>* c2, S2<int>* s2)
{
    C2<char>* otherC2 = C2<char>::init(c2->getData());
    if (c2->getData() != otherC2->getData())
        return 1;
    S2<int>* otherS2 = S2<int>::init(s2->value());
    if (s2->value() != otherS2->value())
        return 2;
    return 0;
}

namespace foo
{
    namespace bar
    {
        namespace baz
        {
            int doStuff(int i)
            {
                return i * 2;
            }
        }
    }
}

#ifndef __DMC__ // DMC doesn't support c++11
template<typename ...T> void foovargs(T... args);

void test40()
{
    foovargs<int, float>(1, 2.0f);
    char c = 'a';
    foovargs<char*>(&c);
}

template<typename T, typename ...Args>
void make_shared_poc(Args&... args);

void test41()
{
    int a = 1;
    int b = 2;
    make_shared_poc<int, int, int>(a, b);

}
#endif
