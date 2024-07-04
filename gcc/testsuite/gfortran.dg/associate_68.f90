! { dg-do run }
! Test the fix for PR114280 in which inquiry references of associate names
! of as yet unparsed function selectors failed.
! Contributed by Steve Kargl  <>
program paul2
  implicit none
  type t
     real :: re
  end type t
  real :: comp = 1, repart = 10, impart =100
  call foo
contains
  subroutine foo ()
    associate (x => bar1())
! 'x' identified as complex from outset
      if (int(x%im) .ne. 100) stop 1         ! Has no IMPLICIT type
      if (int(x%re) .ne. 10) stop 2
    end associate

    associate (x => bar1())
! 'x' identified as derived then corrected to complex
      if (int(x%re) .ne. 11) stop 3          ! Has no IMPLICIT type
      if (int(x%im) .ne. 101) stop 4
      if (x%kind .ne. kind(1.0)) stop 5
    end associate

    associate (x => bar1())
      if (x%kind .ne. kind(1.0)) stop 6      ! Invalid character in name
    end associate

    associate (x => bar2())
      if (int(x%re) .ne. 1) stop 7           ! Invalid character in name
    end associate

    associate (xx => bar3())
      if (xx%len .ne. 8) stop 8               ! Has no IMPLICIT type
      if (trim (xx) .ne. "Nice one") stop 9
      if (xx(6:8) .ne. "one") stop 10
    end associate

! Now check the array versions
    associate (x => bar4())
      if (any (int(abs (x(:) + 2.0)) .ne. [104,105])) stop 0
      if (int(x(2)%re) .ne. 14) stop 11
      if (any (int(x%im) .ne. [103,104])) stop 12
      if (any (int(abs(x)) .ne. [103,104])) stop 13
    end associate

    associate (x => bar5())
      if (x(:)%kind .ne. kind("A")) stop 14
      if (x(2)%len .ne. 4) stop 15
      if (x%len .ne. 4) stop 16
      if (x(2)(1:3) .ne. "two") stop 17
      if (any(x .ne. ["one ", "two "])) stop 18
    end associate
  end
  complex function bar1 ()
    bar1 = cmplx(repart, impart)
    repart = repart + 1
    impart = impart + 1
  end
  type(t) function bar2 ()
    bar2% re = comp
    comp = comp + 1
  end
  character(8) function bar3 ()
    bar3 = "Nice one!"
  end
  function bar4 () result (res)
    complex, allocatable, dimension(:) :: res
    res = [cmplx(repart, impart),cmplx(repart+1, impart+1)]
    repart = repart + 2
    impart = impart + 2
  end
  function bar5 () result (res)
    character(4), allocatable, dimension(:) :: res
    res = ["one ", "two "]
  end
end
