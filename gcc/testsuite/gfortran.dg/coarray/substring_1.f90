!{ dg-do run }

! Test PR51815 is fixed
! Contributed by Bill Long  <longb ad cray dot com>

PROGRAM pr51815
   implicit none
   character(10) :: s[*]
   character(18) :: d = 'ABCDEFGHIJKLMNOPQR'
   integer       :: img

   img = this_image()
   s = d(img:img+9)
   if (img == 1 .and. s(2:4) /= 'BCD') stop 1
END PROGRAM

