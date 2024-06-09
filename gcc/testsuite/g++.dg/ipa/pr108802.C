/* { dg-do compile  } */
/* { dg-options "-O2 -std=c++14 -fdump-ipa-inline -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */

struct A {
    int interesting(int x) { return 2 * x; }
};

int f1() {
    A a;
    return [&](auto&& f) { return (a.*f)(42); } (&A::interesting);
}

/* { dg-final { scan-ipa-dump "A::interesting\[^\\n\]*inline copy in int f1"  "inline"  } } */
