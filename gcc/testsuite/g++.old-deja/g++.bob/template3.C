// { dg-do assemble  }
// prms-id: 9979

template < class Referencee >
class Referencer
{
public:
    Referencer() {}
};

template <class T>
class List
{
public:
    List() {}
};

template<class T, class KEY>
class Dictionary
{
public:
    Dictionary() : i_buckets (new List<T>[1234]) {}
    ~Dictionary() { delete [] i_buckets; }

    List<T> *		i_buckets;
};

class Exchangeable {};
class ExchangeableHandle {};

class ExchangeableList
    : public Dictionary<Referencer<Exchangeable>, ExchangeableHandle>
{
public:
    ExchangeableList(int size=0);
};

class ObjectExchange
{
public:
    ObjectExchange() {};

    ExchangeableList	i_theWatchList; // Instruments being monitored
};

int
main()
{
}
