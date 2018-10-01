! { dg-do compile }
! PR 87395 - this used to ICE
module mo
  integer, save :: x
contains
  subroutine foo
    x = 42
    call bar(x)
  contains
    subroutine bar(y)
      integer, intent(out) :: y
    end subroutine bar
  end subroutine foo
end module mo
