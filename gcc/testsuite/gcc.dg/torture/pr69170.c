/* { dg-do compile } */

typedef long sha2_word_t;
typedef struct {
    sha2_word_t length_upper, length_lower;
    char buf[];
} hash_state;
int a;
extern hash_state b;
void fn1()
{
  a = 0;
  for (; a < 8; a++)
    b.buf[a + 1024 / 8] = b.length_upper >> (1 - a) * 5;
  a = 0;
  for (; a < 8; a++)
    b.buf[a + 1024 / 8 + 8] = b.length_lower >> (1 - a) * 5;
}
