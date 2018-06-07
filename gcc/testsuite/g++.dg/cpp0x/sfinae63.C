// PR c++/61806
// { dg-do compile { target c++11 } }

struct true_type 
{
  static const bool value = true;
};

struct false_type 
{
  static const bool value = false;
};

template<class T>
T&& declval();

template<typename> struct check { typedef void type; };

template<typename T, typename Enable = void>
struct has_public_f : false_type {};

template<typename T>
struct has_public_f<
    T,
    typename check<
        decltype(
            declval<T&>().f()
        )
    >::type
> : true_type {};


struct Spub  { public: void f(); };
struct Spriv { private: void f(); };

static_assert( has_public_f<Spub>::value, "Ouch");
static_assert(!has_public_f<Spriv>::value, "Ouch");

int main() {}
