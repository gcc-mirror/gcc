! { dg-do compile }
!
! Test the fix for PR84115 comment #1 (except for s1(x)!).
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
    associate (y => x//x)   ! { dg-error "type character and non-constant length" }
      print *, y
    end associate
  end

  subroutine s2(x)
    character(:), allocatable :: x
    associate (y => [x])
      print *, y
    end associate
  end

  subroutine s3(x)
    character(:), allocatable :: x
    associate (y => [x,x])
      print *, y
    end associate
  end
end
