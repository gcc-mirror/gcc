/* { dg-do compile } */

void *a, *b;
struct pt3_i2cbuf {
    int num_cmds;
} c;
void *memcpy(void *, void *, __SIZE_TYPE__);
void put_stop();
void translate(struct pt3_i2cbuf *p1, int p2)
{
  p1->num_cmds = 0;
  if (p2)
    put_stop();
}
void pt3_i2c_master_xfer(int p1)
{
  translate(&c, p1);
  memcpy(a, b, c.num_cmds);
  for (; p1;)
    ;
}
