// { dg-do assemble  }
// GROUPS passed overloading
typedef struct{double re,im;} complex;
class Complex{public:double re,im;
    inline void operator=(Complex&X){re=X.re; im=X.im;}};
void zxcvbnm(int n,...){n=1;}
int main(){complex c; Complex C;
zxcvbnm(1,c);
zxcvbnm(1,C);} // { dg-warning "" } cannot pass non pod
