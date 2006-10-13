// { dg-do assemble  }
// GROUPS passed scoping
class Integer {
public:
    int i;
};

class Type {
    enum Klasse { ENUM, INTEGER };

    class Description {
    public:
        
    };

    class Integer: public Description {
    public:
        ::Integer low;
        ::Integer high;
    };
};
