// Tests whether g++ can handle large number of operators
// { dg-do compile }

#define OP0(n) struct I##n { int i; }; operator I##n ();
#define OP1(n) OP0(n)
#define OP2(n) OP1(n##0) OP1(n##1) OP1(n##2) OP1(n##3) OP1(n##4)
#define OP3(n) OP2(n##0) OP2(n##1) OP2(n##2) OP2(n##3) OP2(n##4)
struct S {
  OP3(0) OP3(1) OP3(2) OP3(3) OP3(4) OP3(5)
};
