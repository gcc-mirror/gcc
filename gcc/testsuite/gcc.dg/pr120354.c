/* PR120354: Test for -Wflex-array-member-not-at-end on union with 
   flexible array members.  */ 
/* { dg-do compile } */
/* { dg-options "-Wflex-array-member-not-at-end" } */

struct P {};
union L {};

union X {
    int x[];
    struct P y;
};

struct T {
    union X x;	/* { dg-warning "structure containing a flexible array member is not at the end of another structure" } */
    int plug;
};

struct Q {
    int len;
    int data[];
};

union Y {
    struct Q q;
    union L y;
};

struct S {
    union Y y;  /* { dg-warning "structure containing a flexible array member is not at the end of another structure" } */
    int plug;
};

