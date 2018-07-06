/* { dg-do compile } */

struct struct1 { char a;};
struct struct2 { char a, b;};
struct struct3 { char a, b, c; };
struct struct4 { char a, b, c, d; };
struct struct5 { char a, b, c, d, e; };
struct struct6 { char a, b, c, d, e, f; };
struct struct7 { char a, b, c, d, e, f, g; };
struct struct8 { char a, b, c, d, e, f, g, h; };
struct struct9 { char a, b, c, d, e, f, g, h, i; };
struct struct10 { char a, b, c, d, e, f, g, h, i, j; };
struct struct11 { char a, b, c, d, e, f, g, h, i, j, k; };
struct struct12 { char a, b, c, d, e, f, g, h, i, j, k, l; };
struct struct13 { char a, b, c, d, e, f, g, h, i, j, k, l, m; };
struct struct14 { char a, b, c, d, e, f, g, h, i, j, k, l, m, n; };
struct struct15 { char a, b, c, d, e, f, g, h, i, j, k, l, m, n, o; };
struct struct16 { char a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p; };

struct struct1 foo1 = {'1'};
struct struct2 foo2 = { 'a', 'b'};
struct struct3 foo3 = { 'A', 'B', 'C'};
struct struct4 foo4 = {'1', '2', '3', '4'};
struct struct5 foo5 = {'a', 'b', 'c', 'd', 'e'};
struct struct6 foo6 = {'A', 'B', 'C', 'D', 'E', 'F'};
struct struct7 foo7 = {'1', '2', '3', '4', '5', '6', '7'};
struct struct8 foo8 = {'1', '2', '3', '4', '5', '6', '7', '8'};
struct struct9 foo9 = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'};
struct struct10 foo10 = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'};
struct struct11 foo11 = {
  '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B'};
struct struct12 foo12 = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L'};
struct struct13 foo13 = {
  'a','b','c','d','e','f','g','h','i','j','k','l','m'};
struct struct14 foo14 = {
  'a','b','c','d','e','f','g','h','i','j','k','l','m','n'};
struct struct15 foo15 = {
  'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o'};
struct struct16 foo16 = {
  'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p'};

#define FUN(x) void fun##x ()\
{ \
  volatile struct struct##x var##x = foo##x; \
}

FUN(1)
FUN(2)
FUN(3)
FUN(4)
FUN(5)
FUN(6)
FUN(7)
FUN(8)
FUN(9)
FUN(10)
FUN(11)
FUN(12)
FUN(13)
FUN(14)
FUN(15)
FUN(16)

/* { dg-final { scan-assembler-times {ldr\s} 18 } } */
/* { dg-final { scan-assembler-times {ldrb} 4 } } */
/* { dg-final { scan-assembler-times {ldrh} 4 } } */
/* { dg-final { scan-assembler-times {ldp} 1 } } */
