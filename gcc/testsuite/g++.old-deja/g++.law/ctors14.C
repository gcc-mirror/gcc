// { dg-do assemble  }
// GROUPS passed constructors
// ctor file
// Message-Id: <199301310203.AA22417@oak.ucsc.edu>
// From: "Dean R. E. Long" <dlong@cse.ucsc.edu>
// Subject: problems with default constructors
// Date: Sat, 30 Jan 1993 18:03:32 -0800

class A0 {
};

class A1 {
public:
    A1(void) {}
};

class A2 {
    A1 a;
};

class B0 : public A0 {
public:
    B0(void) {}
    B0(int)  {}
    B0(char) : A0() {}
    B0(short) : A0() {}
};

class B1 : public A1 {
public:
};

class B2 : public A2 {
public:
    B2(void) : A2() {}
    B2(int)  : A2() {}
};

class C : public B1 {
public:
    C(void) : B1() {}
    C(int)  : B1() {}
};
