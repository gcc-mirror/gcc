// Build don't link:
// Special g++ Options: -O2
// Origin: Jakub Jelinek <jakub@redhat.com>

void bar(int);
void foo(int x)
{
  bar(({ int y; y = x; }));
}
