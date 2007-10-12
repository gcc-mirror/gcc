/* { dg-do run } */
/* { dg-options "-O2" } */

struct rtx_def
{
  int mode : 8;
  union {
    long long hwint[1];
  } u;
};
typedef struct rtx_def *rtx;

void useme (long long int i) __attribute__((noinline));
void do_something (void *) __attribute__((noinline));
rtx get_rtx (int) __attribute__((noinline));
void output_766 (rtx *operands,int) __attribute__((noinline));

int target_flags;

int global_count;
void useme (long long int i __attribute__((unused)))
{
  global_count++;
}

rtx get_rtx (int i) { return i?0:0; }
void do_something (void *p __attribute__((unused))) { global_count++;}

void
output_766 (rtx *operands, int ival)
{
  int i;
  for (i = ival; i >= ((operands[2])->u.hwint[0]); i--)
    {
      useme(i*16);
      operands[5] = get_rtx (i <= 7 ? i+1 : i+2);
      operands[4]->mode = 15;
      do_something (operands);
    }
}

extern void abort(void);
int main(void)
{
  rtx operands[6];
  struct rtx_def rtx2, rtx4;
  target_flags = 1 << 2;
  operands[0] = 0;
  operands[1] = 0;
  operands[3] = 0;
  operands[5] = 0;
  operands[2] = &rtx2;
  operands[4] = &rtx4;
  rtx2.u.hwint[0] = 0;
  output_766 (operands, 7);
  if (global_count != 16)
    abort();
  return 0;
}
