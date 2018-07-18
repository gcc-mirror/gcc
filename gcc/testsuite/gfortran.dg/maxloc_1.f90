! { dg-do run }
  integer :: a(3), n
  a(1) = -huge(n)
  a(2) = -huge(n)
  a(3) = -huge(n)
  a(1) = a(1) - 1
  a(2) = a(2) - 1
  a(3) = a(3) - 1
  n = maxloc (a, dim = 1)
  if (n .ne. 1) STOP 1
  a(2) = -huge(n)
  n = maxloc (a, dim = 1)
  if (n .ne. 2) STOP 2
end
