! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

program main
  real, dimension(100) :: a,b
  call random_number(a)
  do concurrent (i=1:100)
     b(i) = a(i)*a(i)
  end do
  print *,sum(a)
end program main

! { dg-final { scan-tree-dump-times "ivdep" 1 "original" } }
