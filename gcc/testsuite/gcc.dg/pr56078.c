/* PR c/56078 */
/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

typedef __SIZE_TYPE__ size_t;
extern int memcmp (const void *, const void *, size_t);
extern void abort (void);

struct T { int a; char b[]; };
struct T t1 = { .a = 1, .b = "abcd", .b[0] = '2' };
struct T t2 = { .a = 1, .b = "2bcd" };
struct T t3 = { .a = 1, .b[2] = 'a' };
struct T t4 = { .a = 1, .b = { '\0', '\0', 'a' } };
struct T t5 = { .a = 1, .b = { [0] = 'a', [1] = 'b', [2] = 'c' } };
struct T t6 = { .a = 1, .b[2] = 'c', .b[1] = 'x', .b[0] = 'a', .b[1] = 'b' };

int
main ()
{
  if (memcmp (t1.b, t2.b, sizeof ("abcd")) != 0
      || memcmp (t3.b, t4.b, 3) != 0
      || memcmp (t5.b, t6.b, 3) != 0)
    abort ();
  return 0;
}
