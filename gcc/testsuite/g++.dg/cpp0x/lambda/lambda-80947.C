// { dg-do compile { target c++11 } }
// { dg-require-visibility "" }
// { dg-options "-fvisibility=hidden" }

template<class T>
class MyClass  {
public:
    MyClass() {
        auto outer = [this]()
            {
                auto fn = [this]   { };
            };
    }
};

int main() { MyClass<int> r; }
