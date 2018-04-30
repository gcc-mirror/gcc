/* { dg-do compile } */
/* { dg-additional-options "-Wno-long-long" } */

__extension__ _Complex float cf = 1.0f + 2.0if;
/* { dg-final { scan-assembler ".align 4 .u32 cf\\\[2\\\] = { 1065353216, 1073741824 };" } } */

__extension__ _Complex double cd = 3.0 + 4.0i;
/* { dg-final { scan-assembler ".align 8 .u64 cd\\\[2\\\] = { 4613937818241073152, 4616189618054758400 };" } } */

long long la[2] = 
  {0x0102030405060708ll,
   0x1112131415161718ll};
/* { dg-final { scan-assembler ".align 8 .u64 la\\\[2\\\] = { 72623859790382856, 1230066625199609624 };" } } */

struct six 
{
  char a;
  short b, c;
};

struct six six1 = {1, 2, 3};
/* { dg-final { scan-assembler ".align 2 .u16 six1\\\[3\\\] = { 1, 2, 3 };" } } */

struct six six2[2] = {{4, 5, 6}, {7, 8, 9}};
/* { dg-final { scan-assembler ".align 2 .u16 six2\\\[6\\\] = { 4, 5, 6, 7, 8, 9 };" } } */

struct __attribute__((packed)) five 
{
  char a;
  int b;
};

struct five five1 = {10, 11};
/* { dg-final { scan-assembler ".align 1 .u8 five1\\\[5\\\] = { 10, 11, 0, 0, 0 };" } } */

struct five five2[2] = {{12, 13}, {14, 15}};
/* { dg-final { scan-assembler ".align 1 .u8 five2\\\[10\\\] = { 12, 13, 0, 0, 0, 14, 15, 0, 0, 0 };" } } */

int __attribute__((vector_size(16))) vi = {16, 17, 18, 19};
/* { dg-final { scan-assembler ".align 16 .u32 vi\\\[4\\\] = { 16, 17, 18, 19 };" } } */

typedef int __attribute ((mode(TI))) ti_t;

ti_t ti = (ti_t)0x2122232425262728ll + ((ti_t)0x3132333435363738ll << 64);
/* { dg-final { scan-assembler ".align 8 .u64 ti\\\[2\\\] = { 2387509390608836392, 3544952156018063160 };" } } */
