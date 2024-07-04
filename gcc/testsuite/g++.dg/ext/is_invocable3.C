// { dg-do compile { target c++11 } }
// __is_invocable should handle incomplete class correctly.

#define SA(X) static_assert((X),#X)

struct Incomplete;

SA( ! __is_invocable( Incomplete ) ); // { dg-error "incomplete type" }
SA( ! __is_invocable( Incomplete, int ) ); // { dg-error "incomplete type" }

SA( ! __is_invocable( int, Incomplete, int ) ); // { dg-error "incomplete type" }
SA( ! __is_invocable( int, Incomplete ) ); // { dg-error "incomplete type" }

SA( ! __is_invocable( Incomplete, Incomplete() ) ); // { dg-error "incomplete type" }
SA( ! __is_invocable( Incomplete, Incomplete(int), int ) ); // { dg-error "incomplete type" }
SA( ! __is_invocable( Incomplete, Incomplete(int, int), int, int ) ); // { dg-error "incomplete type" }

SA( ! __is_invocable( Incomplete, Incomplete(), int, int ) ); // { dg-error "incomplete type" }

SA( ! __is_invocable( int(Incomplete), Incomplete ) ); // { dg-error "incomplete type" }
SA( ! __is_invocable( int(int, Incomplete), int, Incomplete ) ); // { dg-error "incomplete type" }
SA( ! __is_invocable( int(int, Incomplete), Incomplete, int ) ); // { dg-error "incomplete type" }

SA(   __is_invocable( int(Incomplete&), Incomplete& ) ); // { dg-bogus "incomplete type" }
SA(   __is_invocable( int(int, Incomplete&), int, Incomplete& ) ); // { dg-bogus "incomplete type" }

SA(   __is_invocable( int(Incomplete&&), Incomplete&& ) ); // { dg-bogus "incomplete type" }
SA(   __is_invocable( int(int, Incomplete&&), int, Incomplete&& ) ); // { dg-bogus "incomplete type" }

SA(   __is_invocable( int(const Incomplete&&), const Incomplete&& ) ); // { dg-bogus "incomplete type" }
SA(   __is_invocable( int(int, const Incomplete&&), int, const Incomplete&& ) ); // { dg-bogus "incomplete type" }

SA(   __is_invocable( int(const Incomplete&), const Incomplete& ) ); // { dg-bogus "incomplete type" }
SA(   __is_invocable( int(int, const Incomplete&), int, const Incomplete& ) ); // { dg-bogus "incomplete type" }

SA(   __is_invocable( int(const Incomplete&), Incomplete& ) ); // { dg-bogus "incomplete type" }
SA(   __is_invocable( int(int, const Incomplete&), int, Incomplete& ) ); // { dg-bogus "incomplete type" }

SA(   __is_invocable( int Incomplete::*, const Incomplete& ) ); // { dg-bogus "incomplete type" }
SA( ! __is_invocable( void (Incomplete::*)(long&), const Incomplete*, long& ) ); // { dg-bogus "incomplete type" }
SA(   __is_invocable( void (Incomplete::*)(long&) const, Incomplete*, long& ) ); // { dg-bogus "incomplete type" }

template <typename T>
struct Holder { T t; };

SA(   __is_invocable( int(Holder<Incomplete>&), Holder<Incomplete>& ) ); // { dg-bogus "incomplete type" }

// Define Incomplete, which is now not incomplete.
struct Incomplete { void operator()(); };

SA(   __is_invocable( Incomplete ) ); // { dg-bogus "incomplete type" }
