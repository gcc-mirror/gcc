// { dg-do compile }

namespace boost_no_member_template_friends{

template <class T>
class foobar;

template <class T>
class foo
{
private:
   template<typename Y> friend class foobar;
   template<typename Y> friend class foo;
   template<typename Y> friend bool must_be_friend_proc(const foo<Y>& f);
   int i;
public:
   foo(){ i = 0; }
   template <class U>
   foo(const foo<U>& f){ i = f.i; }
};

template <class T>
class foo;

template <class T>
bool must_be_friend_proc(const foo<T>& f);

template <class T>
bool must_be_friend_proc(const foo<T>& f)
{ return f.i != 0; }

template <class T>
class foobar
{
   int i;
public:
   template <class U>
   foobar(const foo<U>& f)
   { i = f.i; }
};


int test()
{
   foo<int> fi;
   foo<double> fd(fi);
   (void) &fd;           // avoid "unused variable" warning
   foobar<long> fb(fi);
   (void) &fb;           // avoid "unused variable" warning
   return 0;
}

}
