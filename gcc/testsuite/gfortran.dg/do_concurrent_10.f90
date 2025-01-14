! { dg-do compile }
! { dg-options "-std=f2018" }

program do_concurrent_parsing
  implicit none
  integer :: concurrent, do
  do concurrent = 1, 5
  end do
  do concurrent = 1, 5
  end do
end program do_concurrent_parsing
