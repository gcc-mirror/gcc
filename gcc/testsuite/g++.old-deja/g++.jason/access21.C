// { dg-do assemble  }
// PRMS Id: 6877

typedef __SIZE_TYPE__ size_t;
class aa {
public:
        aa();
        ~aa();
private:
        int iaa;
        void operator delete(void*, size_t);
};

class bb {
public:
        aa caa;
};				// { dg-bogus "" } calling private delete

void
f(){
        bb abb;
}
