! { dg-do run }
! Tests the fix for PR38538, where the character length for the
! argument of 'func' was not calculated.
!
! Contributed by Vivek Rao <vivekrao4@yahoo.com>
!
module abc
  implicit none
contains
  subroutine xmain (i, j)
    integer i, j
    call foo (func ("_"//bar (i)//"x"//bar (j)//"x"), "_abcxabx")  ! original was elemental
    call foo (nfunc("_"//bar (j)//"x"//bar (i)//"x"), "_abxabcx")
  end subroutine xmain
!
  function bar (i) result(yy)
    integer i, j, k
    character (len = i) :: yy(2)
    do j = 1, size (yy, 1)
      do k = 1, i
        yy(j)(k:k) = char (96+k)
      end do
    end do
  end function bar
!
  elemental function func (yy) result(xy)
    character (len = *), intent(in) :: yy
    character (len = len (yy)) :: xy
    xy = yy
  end function func
!
  function nfunc (yy) result(xy)
    character (len = *), intent(in) :: yy(:)
    character (len = len (yy)) :: xy(size (yy))
    xy = yy
  end function nfunc
!
  subroutine foo(cc, teststr)
    character (len=*), intent(in) :: cc(:)
    character (len=*), intent(in) :: teststr
    if (any (cc .ne. teststr)) STOP 1
  end subroutine foo
end module abc

  use abc
  call xmain(3, 2)
end
