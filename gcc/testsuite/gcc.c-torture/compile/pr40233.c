typedef int aligned __attribute__((aligned(64)));
struct Frame {
  aligned i;
};

void foo(struct Frame *p)
{
  aligned *q = &p->i;
  *q = 0;
}
