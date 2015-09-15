! { dg-do compile }
! { dg-options "-Wall" }
! { dg-require-visibility "" }
module foo
  integer, private :: i  ! { dg-warning "Unused PRIVATE" }
  integer, private :: j = 0
contains
  subroutine bar
    j = j + 1
  end subroutine bar
end module foo

module bar
  private
  integer :: i ! { dg-warning "Unused PRIVATE" }
end module bar
