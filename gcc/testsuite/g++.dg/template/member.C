// { dg-do compile }


class BIXSet{
int z[4];
public:
void f(BIXSet &other){
z[0]=other.z[0];
}

};

class TestCase2{
public:
BIXSet a,b;

public:
void run(void){
BIXSet x,y;
process(0,x,y);
}

protected:
template<class BS> void process(const int d,BS &en,BS &lb){
a.f(en);b.f(lb);
}

};
