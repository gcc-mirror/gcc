// Build don't link: 
// GROUPS passed templates
class X {
        const char *fptr;
public:
        X(const char *ptr) { fptr = ptr; }
        operator const char*() { return fptr; }
};

int main(){
        X x1("1234");
        X x2(x1+1);
}
