union {
  char *p;
  float f;
} u;

void
f (void)
{
  u.p = "";
  u.f += 1.1;
}
