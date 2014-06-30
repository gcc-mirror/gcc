// PR c++/61647

class XX;

template<typename Container, typename Key>
struct Accessor;

template<typename Container, typename Key, typename KeyStore = Key>
class Variant {
protected:
    KeyStore index;
    Container state;
public:
    Variant(Container st, const Key& i) : index(i), state(st) {}

    template<typename T>
    operator T() const {
        return Accessor<Container, KeyStore>::template get<T>(state, index);
    }
};

class AutoCleanVariant : public Variant<XX*, int> {
public:
    AutoCleanVariant(XX* st, int i) : Variant<XX*,int>(st,i) {}

    template<typename T>
    operator T() const {
         return Variant<XX*, int>::operator T();
    }
};
