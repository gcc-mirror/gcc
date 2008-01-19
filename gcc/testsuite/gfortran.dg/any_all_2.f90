! { dg-do compile }
! PR 34838 - this failed with "Can't convert LOGICAL(1) to LOGICAL(1)
! Test case contributed by Manfred Schwab.
program main
  Logical(kind=1) :: bmp(1),bmpv(1)

  bmp(1)=.false.
  bmpv(1)=.true.

  if ( ANY(bmp(1:1) .NEQV. bmpv(1:1)) ) then
     print*,"hello"
  end if

  if ( ALL(bmp(1:1) .NEQV. bmpv(1:1)) ) then
     print*,"hello"
  end if

end program main
