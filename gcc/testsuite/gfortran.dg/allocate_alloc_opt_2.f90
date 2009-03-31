! { dg-do compile }
subroutine sub(i, j, err)
   implicit none
   character(len=*), intent(in) :: err
   integer, intent(in) :: j
   integer, intent(in), allocatable :: i(:)
   integer, allocatable :: m(:)
   integer n
   allocate(i(2))                    ! { dg-error "Cannot allocate" "" }
   allocate(m(2), stat=j)            ! { dg-error "cannot be" "" }
   allocate(m(2),stat=n,errmsg=err)  ! { dg-error "cannot be" "" }
end subroutine sub
