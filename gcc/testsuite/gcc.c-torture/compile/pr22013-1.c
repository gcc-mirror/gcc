typedef unsigned short W;
typedef const W *P;

extern void g(P);

void
f ()
{
  const P s = (const W []){ 'R' };
  g (s);
}
