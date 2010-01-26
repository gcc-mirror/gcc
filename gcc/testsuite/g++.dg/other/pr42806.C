// { dg-do compile }
// { dg-options "-O -fcompare-debug" }

struct S1 {
    ~S1() { }
};

struct S2 {
    S1 s1;
    void m();
    ~S2() { m(); }
};

struct S3 {
    S3(int, S2);
};

void foo()
{
  S3(0, S2());
}

