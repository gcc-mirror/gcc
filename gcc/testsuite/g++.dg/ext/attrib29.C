// PR c++/33506
// { dg-do compile }

extern int f1 (char *) __attribute__ ((warn_unused_result));
extern int f2 (char *) throw () __attribute__ ((warn_unused_result));
extern int f2 (char *) throw ();

extern int f3 (char *) __attribute__ ((nonnull (1)));
extern int f4 (char *) throw () __attribute__ ((nonnull (1)));
extern int f4 (char *) throw ();
