! { dg-do compile }
! { dg-options -std=gnu }
! PR50555 synonymous namelist/statement function dummy argument not allowed
subroutine g(k1, k2, k3)
  integer, intent(in) :: k1, k2, k3
  print *, k
end subroutine
function j(k1, k2, k3)
  integer, intent(in) :: k1, k2, k3
  j = 25 * k
end function
program pr50555
  namelist /i/ j
  call g(k,l,i) ! { dg-error "can not be an argument" }
  f(k,l,i)=0    ! { dg-error "can not be an argument" }
  h = j(k,l,i)  ! { dg-error "can not be an argument" }
end program
! Note: -std=gnu needed because line 15 function statement is obsolescent
