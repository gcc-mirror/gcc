/*  Origin PR c++/54372
    { dg-options "-Wunused-local-typedefs" }
    { dg-do compile }
*/

template <typename T>
void f2()
{
    typedef T t __attribute__((unused));
}

class S
{
    template <typename T>
    void f4()
    {
	typedef T t __attribute__((unused));
    }
};

template <typename T>
class tS
{
    void f()
    {
	typedef T t2 __attribute__((unused));
    }

    template <typename U>
    void f2()
    {
	typedef T t1 __attribute__((unused));
	typedef U t2 __attribute__((unused));
    }
};
