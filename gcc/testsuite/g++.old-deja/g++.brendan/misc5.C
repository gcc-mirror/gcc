// Build don't link: 
// GROUPS passed miscellaneous
class A {
    static A aa[2];
};

A A::aa[2]; // should be completely legal
