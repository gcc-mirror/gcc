typedef struct _A A;
typedef struct _A B;

void some_function(B *b);

class AClass {

public:
  operator A*() { return 0;}

};

class BClass :public AClass {

public:
  operator B*() { return 0;}

};

int main(int argc, char **argv) {
  BClass b;
  some_function(b);
}
