/* { dg-do compile } */

#pragma pack(push)
struct a {
  volatile signed b : 8;
};
#pragma pack(pop)
int c;
static struct a d = {5};
void e() {
f:
  for (c = 8; c < 55; ++c)
    if (!d.b)
      goto f;
}
