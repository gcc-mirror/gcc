struct c {
  double a;
} __attribute((packed)) __attribute((aligned));

void f(struct c *, struct c);

void g(struct c *ptr)
{
  ptr++;
  f(ptr, *ptr);
}
