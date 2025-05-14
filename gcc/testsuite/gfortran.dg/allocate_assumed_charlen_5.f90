! { dg-do compile }
! PR fortran/119338 - check F2003:C626

module m
  implicit none
contains
  subroutine sub (s, c)
    character(len=*), allocatable, intent(out) :: s(:)
    character(len=*), allocatable, intent(out) :: c
    allocate(s(5))                      ! OK
    allocate(c)                         ! OK
    allocate(character(len=*)  :: s(5)) ! OK
    allocate(character(len=*)  :: c)    ! OK
    allocate(character(len=10) :: s(5)) ! { dg-error "shall be an asterisk" }
    allocate(character(len=10) :: c)    ! { dg-error "shall be an asterisk" }
  end subroutine sub
end module m
