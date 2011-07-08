/* { dg-do run } */

#include <stdlib.h>

struct null_type {};

inline const null_type cnull() { return null_type(); }

template <class TT> struct cons;
class tuple;

template< int N >
struct get_class {
  template<class TT >
  inline static int& get(cons<TT>& t)
  {
    return get_class<N-1>::template get(t.tail);
  }
};

template<>
struct get_class<0> {
  template<class TT>
  inline static int& get(cons<TT>& t)
  {
    return t.head;
  }
};

template<int N, class T>
struct element
{
private:
  typedef typename T::tail_type Next;
public:
  typedef typename element<N-1, Next>::type type;
};

template<class T>
struct element<0,T>
{
  typedef int type;
};

template<int N, class TT>
inline int& get(cons<TT>& c) {
  return get_class<N>::template get(c);
}

template <class TT>
struct cons {
  typedef TT tail_type;

  int head;
  tail_type tail;

  cons() : head(), tail() {}

  template <class T1, class T2, class T3, class T4>
  cons( T1& t1, T2& t2, T3& t3, T4& t4 )
    : head (t1),
      tail (t2, t3, t4, cnull())
      {}
};

template <>
struct cons<null_type> {
  typedef null_type tail_type;

  int head;

  cons() : head() {}

  template<class T1>
  cons(T1& t1, const null_type&, const null_type&, const null_type&)
  : head (t1) {}
};

template <class T0, class T1, class T2, class T3>
struct map_tuple_to_cons
{
  typedef cons<typename map_tuple_to_cons<T1, T2, T3, null_type>::type> type;
};

template <>
struct map_tuple_to_cons<null_type, null_type, null_type, null_type>
{
  typedef null_type type;
};

class tuple :
  public map_tuple_to_cons<int, int, int, int>::type
{
public:
  typedef typename
    map_tuple_to_cons<int, int, int, int>::type inherited;

  tuple(const int &t0,
        const int &t1,
        const int &t2,
        const int &t3)
    : inherited(t0, t1, t2, t3) {}
};

void foo(void (*boo)(int, int, int, int), tuple t)
{
  boo(get<0>(t), get<1>(t), get<2>(t), get<3>(t));
}

int tailcalled_t1;
int tailcalled_t2;
int tailcalled_t3;
int tailcalled_t4;

void print(int t1, int t2, int t3, int t4)
{
  tailcalled_t1 = t1;
  tailcalled_t2 = t2;
  tailcalled_t3 = t3;
  tailcalled_t4 = t4;
}

int main ()
{
  tuple t(1,2,3,4);
  foo(print, t);

  if( (get<0>(t) != tailcalled_t1)
    ||(get<1>(t) != tailcalled_t2)
    ||(get<2>(t) != tailcalled_t3)
      ||(get<3>(t) != tailcalled_t4))
      abort();

  return 0;
}
