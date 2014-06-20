// PR c++/61556
// { dg-do compile { target c++11 } }

class ValueType {
public:
    constexpr operator int() const {return m_ID;};
    constexpr ValueType(const int v)
        : m_ID(v) {}
private:
    int m_ID;
};

class ValueTypeEnum {
public:
    static constexpr ValueType doubleval = ValueType(1);
};

template <int format>
class ValueTypeInfo {
};

template <typename Format>
class FillFunctor {
public:
    FillFunctor() {
        ValueTypeInfo<ValueTypeEnum::doubleval> v;
    }
};

int main() {
    ValueTypeInfo<ValueTypeEnum::doubleval> v;
}
