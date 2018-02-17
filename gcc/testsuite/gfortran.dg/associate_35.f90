! { dg-do run }
!
! Test the fix for PR84115 comment #1.
!
! Contributed by G Steinmetz  <gscfq@t-online.de>
!
  character(:), allocatable :: dum
  dum = "s1"
  call s1 (dum)
  dum = "s2"
  call s2 (dum)
  dum = "s3"
  call s3 (dum)
contains
  subroutine s1(x)
    character(:), allocatable :: x
    associate (y => x//x)
      if (y .ne. x//x) stop 1
    end associate
  end

  subroutine s2(x)
    character(:), allocatable :: x
    associate (y => [x])
      if (any(y .ne. [x])) stop 2
    end associate
  end

  subroutine s3(x)
    character(:), allocatable :: x
    associate (y => [x,x])
      if (any(y .ne. [x,x])) stop 3
    end associate
  end
end
