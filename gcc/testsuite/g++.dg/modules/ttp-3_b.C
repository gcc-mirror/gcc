// { dg-additional-options -fmodules-ts }
module foo;


template <typename> class Y;

static_assert (sizeof (TPL<Y<char>, Y>::type) == 1);
static_assert (sizeof (TPL<Y<int>, Y>::type) != 1, "where's my specialization?");
