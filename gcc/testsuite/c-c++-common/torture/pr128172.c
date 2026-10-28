/* { dg-do compile } */
/* { dg-additional-options "-fexceptions" } */

void abort(void);
static int cnt;
static void __attribute__((noinline)) die(void) { cnt++; abort(); }
typedef void (*fp_t)(void);
static void cl(fp_t *q)
{
  (*q)();
}
void f(fp_t p)
{
  if (p != die)
    return;
  fp_t x __attribute__((cleanup(cl))) = p; 
  p();
}
fp_t get(void)
{
  return die;
}
