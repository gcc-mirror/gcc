// PR c++/33506
// { dg-do compile { target c++11 } }

extern int f1 [[gnu::warn_unused_result]] (char *) ;
extern int f2 [[gnu::warn_unused_result]] (char *) throw () ;
extern int f2 (char *) throw ();

extern int f3 [[gnu::nonnull (1)]] (char *) ;
extern int f4 [[gnu::nonnull (1)]] (char *) throw ();
extern int f4 (char *) throw ();
