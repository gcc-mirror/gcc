// Build don't link: 

template <class T> class temp1
{
public:
        T tvar;
};


template <class T2> class temp2
{
public :
        temp1<T2> t1var;
};


temp1<int> temp1var;
temp2<int> temp2var;
