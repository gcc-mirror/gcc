! { dg-do compile }
! Tests fix for PR20838 - would ICE with vector subscript in 
! pointer assignment.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  integer, parameter, dimension(3) :: i = (/2,1,3/)
  integer, dimension(3), target   :: tar
  integer, dimension(2, 3), target   :: tar2
  integer, dimension(:), pointer  :: ptr
  ptr => tar
  ptr => tar(3:1:-1)
  ptr => tar(i)     ! { dg-error "with vector subscript" }
  ptr => tar2(1, :)
  ptr => tar2(2, i) ! { dg-error "with vector subscript" }
  end

