! { dg-do compile }
subroutine sub(i, j, err)
   implicit none
   character(len=*), intent(in) :: err
   integer, intent(in) :: j
   integer, intent(in), allocatable :: i(:)
   integer, allocatable :: m(:)
   integer n
   deallocate(i)                    ! { dg-error "Cannot deallocate" "" }
   deallocate(m, stat=j)            ! { dg-error "cannot be" "" }
   deallocate(m,stat=n,errmsg=err)  ! { dg-error "cannot be" "" }
end subroutine sub
