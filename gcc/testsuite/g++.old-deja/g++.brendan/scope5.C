// Build don't link: 
// GROUPS passed scoping
class Integer {
public:
    int i;
};

class Type {
    enum Class { ENUM, INTEGER };

    class Description {
    public:
        
    };

    class Integer: public Description {
    public:
        ::Integer low;
        ::Integer high;
    };
};
