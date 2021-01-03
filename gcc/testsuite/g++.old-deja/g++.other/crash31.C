// { dg-do assemble }
// Origin: Jakub Jelinek <jakub@redhat.com>

namespace bar
{
struct foo
{
  foo();
}; // { dg-error "3:expected"  }
