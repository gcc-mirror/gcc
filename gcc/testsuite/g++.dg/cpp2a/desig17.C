// PR c++/87032
// { dg-do compile { target c++20 } }

struct f1 {int x,y;};
struct f2 {int x,y,z,t;};

struct T {
const char * name;
union {
       struct f1  fn1;
       struct f2  fn2;
} d;
};

extern "C" void p(struct T);

int main(){
p({"%x",{.fn2={1,2,3,4}}});
}
