! PR fortran/88304

module pr88304
  implicit none
  type t
     integer :: b = -1
  end type t
contains
  subroutine f1 (x, y)
    integer, intent(out) :: x, y
    x = 5
    y = 6
  end subroutine f1
  subroutine f2 ()
    type(t) :: x
    integer :: y
    call f3
    if (x%b .ne. 5 .or. y .ne. 6) stop 1
  contains
    subroutine f3
      call f1 (x%b, y)
    end subroutine f3
  end subroutine f2
end module pr88304
