typedef const __SVInt8_t foo;
typedef volatile foo bar;

foo f (foo x) { return x; }
bar g (bar x) { return x; }

/* { dg-final { scan-assembler {_Z1fu10__SVInt8_t:\n} } } */
/* { dg-final { scan-assembler {_Z1gu10__SVInt8_t:\n} } } */
