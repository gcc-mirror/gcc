! { dg-do compile }
! PR 58009 - If a vector subscript has two or more elements with the
! same value, an array section with that vector subscript
! shall not appear in a variable definition context.

program main
  real, dimension(4) :: a,b
  real, dimension(1,4) :: c
  read (*,*) a([1,2,3,2]),i ! { dg-error "Elements with the same value" }
  read (*,*) c(1,[1,2,3,2]),i ! { dg-error "Elements with the same value" }
  b([1+i,1,i+1,2]) = a      ! { dg-error "Elements with the same value" }
  c(1,[1+i,1,i+1,2]) = a    ! { dg-error "Elements with the same value" }
  call foo (a([4,2,1,1]))   ! { dg-error "Elements with the same value" }
  call foo (c(1,[4,2,1,1])) ! { dg-error "Elements with the same value" }
  print *,a,b
contains
  subroutine foo(arg)
    real, intent(inout) :: arg(:)
    arg = arg + 1
  end subroutine foo 
end program main
