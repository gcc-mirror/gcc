#include <vector>

template <class T>
class TPROGRAM
    {
    typedef vector< T > ITEMS;

    class const_iterator
        {
        /*typename*/ ITEMS::const_iterator i;

        const_iterator(const /*typename*/ ITEMS::const_iterator i2) {
            i=i2;
            }
        };
    };
