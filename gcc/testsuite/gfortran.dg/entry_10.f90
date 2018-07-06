! { dg-do run }
! Test fix for PR31474, in which the use of ENTRYs as module
! procedures in a generic interface would cause an internal error.
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>
!
module a
  interface b
    module procedure c, d
  end interface
contains
  real function d (i)
    real c, i
    integer j
    d = 1.0
    return
  entry c (j)
    d = 2.0
  end function
  real function e (i)
    real f, i
    integer j
    e = 3.0
    return
  entry f (j)
    e = 4.0
  end function
end module

  use a
  if (b (1.0) .ne. 1.0) STOP 1
  if (b (1  ) .ne. 2.0) STOP 2
  if (e (1.0) .ne. 3.0) STOP 3
  if (f (1  ) .ne. 4.0) STOP 4
end
