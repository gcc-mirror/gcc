// PR c++/10416
// { dg-options "-Wunused" }

void f () { struct atend { ~atend () { __builtin_printf("leaving f\n"); } } a; }
