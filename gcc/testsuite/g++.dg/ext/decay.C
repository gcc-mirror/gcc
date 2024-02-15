// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

// Positive tests.
using test1_type = __decay(bool);
SA(__is_same(test1_type, bool));

// NB: DR 705.
using test2_type = __decay(const int);
SA(__is_same(test2_type, int));

using test3_type = __decay(int[4]);
SA(__is_same(test3_type, __remove_extent(int[4])*));

using fn_type = void ();
using test4_type = __decay(fn_type);
SA(__is_same(test4_type, __add_pointer(fn_type)));

using cfn_type = void () const;
using test5_type = __decay(cfn_type);
SA(__is_same(test5_type, cfn_type));
