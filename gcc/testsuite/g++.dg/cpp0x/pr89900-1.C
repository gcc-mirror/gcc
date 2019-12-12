// { dg-do compile { target c++11 } }

template<typename SX, typename ...XE> void
fk (XE..., SW);  // { dg-error "12:.SW. has not been declared" }

void
w9 (void)
{
  fk<int> (0);
}
