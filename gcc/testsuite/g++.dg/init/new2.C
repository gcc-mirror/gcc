// Origin: asharji@uwaterloo.ca

// { dg-do compile }

class bar {
  public :
    bar() { }
    void * operator new ( __SIZE_TYPE__ , void * storage ) 
     { return (void *)1;}
};

class foo {
  public:
    void mem ( ) {
        new ( 0 ) bar;
    }
};
