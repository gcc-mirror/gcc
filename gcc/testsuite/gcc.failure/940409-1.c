struct S { volatile int field; };
int f (register struct S arg);
int g (register struct S);
