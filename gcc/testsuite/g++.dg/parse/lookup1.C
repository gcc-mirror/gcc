#include <list>

using namespace std;

template <class T, class Alloc>
class new_list : public list<T, Alloc> {
public:
    typedef typename list<T, Alloc>::iterator iterator;
};
