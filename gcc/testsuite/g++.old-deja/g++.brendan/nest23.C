// Build don't link: 
// GROUPS passed nested-classes
class CS {
public:
    class PS {
    };
};

class NCS: public CS {
public:
    class S: public PS {
    };
};

int i;
