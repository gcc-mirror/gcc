/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details" } */

static const struct {
    int w;
    int h;
} sizes[7] = {
    { 16, 16 },
    { 16,  8 },
    {  8, 16 },
    {  8,  8 },
    {  8,  4 },
    {  4,  8 },
    {  4,  4 }
};

int baz(int, int);

[[gnu::noinline]] void bar(int w, int h)
{
  for (int i = 0; i < w; i++)
    for (int j = 0; i < h; j++)
      baz (i, j);
}

void foo (int index)
{
 int w = sizes[index].w;
 int h = sizes[index].h;

 bar (w, h);
}
/* { dg-final { scan-ipa-dump "irange" "cp" } } */
