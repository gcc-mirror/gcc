// PRMS Id: 6877
// Build don't link:

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
};				// gets bogus error - calling private delete

void
f(){
        bb abb;
}
