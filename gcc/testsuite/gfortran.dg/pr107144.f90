! { dg-do compile }
!
! PR 107144

subroutine s (fn)
  interface
    function fn (n)
      integer :: n
      character(n) :: fn
    end function
  end interface
  if (fn (3) /= 'abc') stop 1
end subroutine
