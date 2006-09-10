struct __attribute__((__packed__)) s { char c; unsigned long long x; };
void __attribute__((__noinline__)) foo (struct s *s) { s->x = 0; }
int main (void) { struct s s = { 1, ~0ULL }; foo (&s); return s.x != 0; }
