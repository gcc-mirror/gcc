! { dg-do compile }
subroutine sub(i, j, err)
   implicit none
   character(len=*), intent(in) :: err
   integer, intent(in) :: j
   integer, intent(in), allocatable :: i(:)
   integer, allocatable :: m(:)
   integer n
   deallocate(i)                    ! { dg-error "variable definition context" }
   deallocate(m, stat=j)            ! { dg-error "variable definition context" }
   deallocate(m,stat=n,errmsg=err)  ! { dg-error "variable definition context" }
end subroutine sub
