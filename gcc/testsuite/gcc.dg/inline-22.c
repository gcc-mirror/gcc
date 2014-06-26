/* { dg-do compile } */
/* { dg-options "-funit-at-a-time -Wno-attributes" } */
/* { dg-add-options bind_pic_locally } */
/* Verify we can inline without a complete prototype and with promoted
   arguments.  See also PR32492.  */
__attribute__((always_inline)) void f1() {}
__attribute__((always_inline)) void f2(char x) {}
void f3() { f1(); f2(0); }
