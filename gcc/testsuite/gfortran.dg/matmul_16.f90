! { dg-do  run }
! { dg-options "-finline-matmul-limit=0" }
! PR 80975 - this did not zero the result array
program bogus_matmul
  implicit none
  real :: M(3,0), v(0), w(3)

  w = 7
  w = matmul(M,v)
  if( any(w .ne. 0) ) then
    call abort
  end if
end program bogus_matmul
