// Origin: asharji@uwaterloo.ca

// { dg-do compile }

typedef __SIZE_TYPE__ size_t;

class bar {
    int i;
  public :
    void * operator new ( size_t , void * storage );
};

class foo {
    int storage[ 5 ];
  public:
    void mem ( ) {
        bar *s = new ( ( void * ) & storage ) bar;
    }
};
