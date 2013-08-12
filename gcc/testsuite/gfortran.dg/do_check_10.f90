! { dg-do compile }
! { dg-options "-Wall -Wno-zerotrip" }
program main
  do i=1,0
    print *,i
  end do
end program main
