// Build don't link: 
// GROUPS passed templates

class String {
        char s[100];
};

template <class Element>
class Art {
public:
        Element *data;
        Art() { data=new Element[100]; }
};

template <class Key,class Value>
class Assoc {
public:
        struct KeyValue {
                Key key;
                Value value;
                int filled;
        };

        Art<KeyValue> data;
        int fill;
};

int main() {
        Assoc<String,String> table;
}
