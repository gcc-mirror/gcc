/* PR target/124495 */
/* { dg-do assemble } */
/* { dg-options "-O2 -std=c++17" } */

/* The exception receiver expands to an ldah/lda pair sharing a !gpdisp
   relocation sequence number.  If the landing pad turns out not to use $29,
   neither half may be deleted on its own.  */

#include <memory>
#include <variant>
#include <vector>

struct S { std::variant<int, std::vector<int>> v; };

S *
f (S *first, S *last, S *result)
{
  return std::uninitialized_copy (first, last, result);
}
