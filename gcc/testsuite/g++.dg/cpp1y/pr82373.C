// PR c++/82373
// { dg-do compile { target c++14 } }

namespace N
{
  int (*fp)(int);
  auto foo(int a)	// { dg-message "In function 'auto N::foo\\(int\\)'" "" { target *-*-* } 0 }
  {
    if (a)
      return fp;
    return nullptr;	// { dg-error "inconsistent deduction for auto return type: 'int \\(\\*\\)\\(int\\)' and then 'std::nullptr_t'" } */
  }
}
int (*fp2)(int);
auto bar(int a)		// { dg-message "In function 'auto bar\\(int\\)'" "" { target *-*-* } 0 }
{
  if (a)
    return fp2;
  return nullptr;	// { dg-error "inconsistent deduction for auto return type: 'int \\(\\*\\)\\(int\\)' and then 'std::nullptr_t'" } */
}
