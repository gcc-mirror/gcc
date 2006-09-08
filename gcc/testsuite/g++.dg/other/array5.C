// Check to make sure changing from an incomplete
// array type to a complete one does not change other
// incomplete array type's bounds.
// { dg-do compile }

extern unsigned char xvalue_store[];
extern unsigned char xvalue_store1[];
unsigned char xvalue_store[7];
unsigned char xvalue_store1[9];
