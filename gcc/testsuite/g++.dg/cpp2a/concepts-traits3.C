// PR c++/100474
// { dg-do compile { target c++20 } }

struct S { S() = delete; S(const S&); };

template<class T>
concept Aggregate = __is_aggregate(T);
// { dg-message "'S' is not an aggregate" "" { target *-*-* } .-1  }

template<class T>
concept TriviallyCopyable = __is_trivially_copyable(T);
// { dg-message "'S' is not trivially copyable" "" { target *-*-* } .-1  }

template<class T, class U>
concept Assignable = __is_assignable(T, U);
// { dg-message "'S' is not assignable from 'int'" "" { target *-*-* } .-1  }

template<class T, class U>
concept TriviallyAssignable = __is_trivially_assignable(T, U);
// { dg-message "'S' is not trivially assignable from 'int'" "" { target *-*-* } .-1  }

template<class T, class U>
concept NothrowAssignable = __is_nothrow_assignable(T, U);
// { dg-message "'S' is not nothrow assignable from 'int'" "" { target *-*-* } .-1  }

template<class T, class... Args>
concept Constructible = __is_constructible(T, Args...);
// { dg-message "'S' is not default constructible" "" { target *-*-* } .-1  }
// { dg-message "'S' is not constructible from 'int'" "" { target *-*-* } .-2  }
// { dg-message "'S' is not constructible from 'int, char'" "" { target *-*-* } .-3  }

template<class T, class... Args>
concept TriviallyConstructible = __is_trivially_constructible(T, Args...);
// { dg-message "'S' is not trivially default constructible" "" { target *-*-* } .-1  }
// { dg-message "'S' is not trivially constructible from 'int'" "" { target *-*-* } .-2  }
// { dg-message "'S' is not trivially constructible from 'int, char'" "" { target *-*-* } .-3  }

template<class T, class... Args>
concept NothrowConstructible = __is_nothrow_constructible(T, Args...);
// { dg-message "'S' is not nothrow default constructible" "" { target *-*-* } .-1  }
// { dg-message "'S' is not nothrow constructible from 'int'" "" { target *-*-* } .-2  }
// { dg-message "'S' is not nothrow constructible from 'int, char'" "" { target *-*-* } .-3  }

template<class T>
concept UniqueObjReps = __has_unique_object_representations(T);
// { dg-message "'S' does not have unique object representations" "" { target *-*-* } .-1  }

static_assert(Aggregate<S>); // { dg-error "assert" }
static_assert(TriviallyCopyable<S>); // { dg-error "assert" }
static_assert(Assignable<S, int>); // { dg-error "assert" }
static_assert(TriviallyAssignable<S, int>); // { dg-error "assert" }
static_assert(NothrowAssignable<S, int>); // { dg-error "assert" }

static_assert(Constructible<S>); // { dg-error "assert" }
static_assert(Constructible<S, int>); // { dg-error "assert" }
static_assert(Constructible<S, int, char>); // { dg-error "assert" }

static_assert(TriviallyConstructible<S>); // { dg-error "assert" }
static_assert(TriviallyConstructible<S, int>); // { dg-error "assert" }
static_assert(TriviallyConstructible<S, int, char>); // { dg-error "assert" }

static_assert(NothrowConstructible<S>); // { dg-error "assert" }
static_assert(NothrowConstructible<S, int>); // { dg-error "assert" }
static_assert(NothrowConstructible<S, int, char>); // { dg-error "assert" }

static_assert(UniqueObjReps<S>); // { dg-error "assert" }
