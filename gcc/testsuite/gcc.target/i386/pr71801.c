/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

struct {
  char uuid[16];
} c;
struct {
  int s_uuid[6];
} a, b;
int bar (void);
static int get_label_uuid(char *p1) {
  __builtin_memcpy(p1, a.s_uuid, sizeof(a));
  if (bar())
    __builtin_memcpy(p1, b.s_uuid, sizeof(b));
  return 0;
}
void uuidcache_addentry(char *p1) { __builtin_memcpy(&c, p1, sizeof(c)); }
void uuidcache_init() {
  char d[sizeof(a) + sizeof(c)];
  get_label_uuid(d);
  uuidcache_addentry(d);
}
