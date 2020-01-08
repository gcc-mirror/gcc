// { dg-do compile { target c++14 } }

void foo();

auto bar(bool b)
{
  if (b)
    return 0;
  return foo(); // { dg-error "13:inconsistent deduction for auto return type: .int. and then .void." }
}
