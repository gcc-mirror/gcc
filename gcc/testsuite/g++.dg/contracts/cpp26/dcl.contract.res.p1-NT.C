// N5008 :
// dcl.contract.res/p1
// The result-name-introducer of a postcondition-specifier is a declaration. The result-name-introducer introduces
// the identifier as the name of a result binding of the associated function. If a postcondition assertion has a
// result-name-introducer and the return type of the function is cv void, the program is ill-formed.
//
// Various tests with non trivial return value identifier
//
// { dg-do compile { target { c++23 && { ! hostedlib } } } }
// { dg-do run     { target { c++23 && hostedlib } } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=enforce" }

int live = 0;

struct NonTrivial{
  NonTrivial(){ ++live; };
  NonTrivial(const NonTrivial&){ ++live;}
  NonTrivial(NonTrivial&&){ ++live;}
  ~NonTrivial(){ --live; };
  int x = 0;
};

struct ThrowOnDestroy {
  bool armed = true;
  ~ThrowOnDestroy() noexcept(false) { if (armed) throw 17; }
};

template<typename T>
bool check(T t){
  return true;
}
struct S{
 NonTrivial f1(const NonTrivial i) post(r: check(i.x > 0) ) { return NonTrivial{};}

 template <typename T>
 NonTrivial f2(const T i) post(r: check(i.x > 0) ) { return NonTrivial{};}

 auto f3(const NonTrivial i) post(r: check(i.x > 0) ) { return NonTrivial{};}

 template <typename T>
 T f4(const T i) post(r: check(i.x > 0) ) { return NonTrivial{};}

 template <typename T>
 auto f5(const T i) post(r: check(i.x > 0) ) { return i;}

 template <typename T>
 auto f6(const T i) post(r: check(i) ) { return i;}

 auto f7(const NonTrivial i) post(r: check(r) ) { return i;}

 NonTrivial f8(bool flag) pre(check(flag))
 { NonTrivial result;
   if (flag) {
     return result;
   }
   int other;  // force a new block
   return result;
 }

 NonTrivial f9(bool arm) pre(check(arm))
 { ThrowOnDestroy guard{arm};
   NonTrivial result;
   return result;
 }

};

template <typename U>
struct S1
{

  struct S2
  {
    NonTrivial
    f1 (const NonTrivial i) post(r: check(i.x > 0) )
      { return NonTrivial
	  {};
      }

      template <typename T>
      NonTrivial
      f2 (const T i) post(r: check(i.x > 0) )
	{ return NonTrivial
	    {};
	}

	auto
	f3 (const NonTrivial i)
	post(r: check(i.x > 0) )
	  { return NonTrivial
	      {};}

	template <typename T>
	T
	f4 (const T i)
	post(r: check(i.x > 0) )
	  { return NonTrivial
	      {};}

	template <typename T>
	auto
	f5 (const T i)
	post(r: check(i.x > 0) )
	  { return i;}

      };

    NonTrivial
    f1 (const NonTrivial i)
    post(r: check(i.x > 0) )
      { S2 s;
	return s.f1(i);
      }

    template <typename T>
    NonTrivial
    f2 (const T i)
    post(r: check(i.x > 0) )
      { S2 s;
	return s.f2(i);
      }

    auto
    f3 (const NonTrivial i)
    post(r: check(i.x > 0) )
      { S2 s;
	return s.f3(i);
      }

    template <typename T>
    T
    f4 (const T i)
    post(r: check(i.x > 0) )
      { S2 s;
	return s.f4(i);
      }

    template <typename T>
    auto
    f5 (const T i)
    post(r: check(i.x > 0) )
      { S2 s;
	return s.f5(i);
      }

    template <typename T>
    auto f6(T flag) pre(check(flag))
    { NonTrivial result;
      if (flag) {
        return result;
      }
      int other;  // force a new block
      return result;
    }

    template <typename T>
    auto f7(T arm) pre(check(arm))
    { ThrowOnDestroy guard{arm};
      NonTrivial result;
      return result;
    }


  };


int main()
{
  {
    S s;
    NonTrivial n;
    s.f1(NonTrivial{});

    s.f2(n);
    s.f3(n);
    s.f4(n);
    s.f5(n);
    s.f6(n);
    s.f7(n);
    s.f8(true);
    s.f8(false);

    try {
      s.f9(true);
    } catch (int) { }
  }

  {
    S1<NonTrivial> s1;
    NonTrivial n;
    s1.f1(n);
    s1.f2(n);
    s1.f3(n);
    s1.f4(n);
    s1.f5(n);
    s1.f6(true);
    s1.f6(false);
    try {
      s1.f7(true);
    } catch (int) { }
  }

  if (live != 0) __builtin_abort();
}
