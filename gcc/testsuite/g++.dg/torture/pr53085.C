// { dg-do compile }
// { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } }
// { dg-options "-fdump-tree-optimized" }

class aa{
    void f();
private:
    volatile int a;
};

void aa::f(){
    a=1;
    a=1;
}

// { dg-final { scan-tree-dump-times "a ={v} 1;" 2 "optimized" } }
