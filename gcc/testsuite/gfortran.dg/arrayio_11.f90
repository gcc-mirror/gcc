! { dg-do run }
! Tests the fix for PR30284, in which the substring plus
! component reference for an internal file would cause an ICE.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>

program gfcbug51
  implicit none

  type :: date_t
    character(len=12) :: date      ! yyyymmddhhmm
  end type date_t

  type year_t
    integer :: year = 0
  end type year_t

  type(date_t) :: file(3)
  type(year_t) :: time(3)

  FILE%date = (/'200612231200', '200712231200', &
                '200812231200'/)

  time = date_to_year (FILE)
  if (any (time%year .ne. (/2006, 2007, 2008/))) call abort ()

  call month_to_date ((/8, 9, 10/), FILE)
  if ( any (file%date .ne. (/'200608231200', '200709231200', &
                             '200810231200'/))) call abort ()

contains

  function date_to_year (d) result (y)
    type(date_t) :: d(3)
    type(year_t) :: y(size (d, 1))
    read (d%date(1:4),'(i4)')  time% year
  end function date_to_year

  subroutine month_to_date (m, d)
    type(date_t) :: d(3)
    integer :: m(:)
    write (d%date(5:6),'(i2.2)')  m
  end subroutine month_to_date

end program gfcbug51
