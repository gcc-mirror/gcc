struct a {
    long a1;
    long a2;
};
struct b {
    struct a b1;
    struct a b2;
};
void bar (struct b *c)
{
  c->b1 = c->b2 = ((struct a) { foo(), 0 });
}
