! { dg-do run }
! Check the fix for PR16206, in which array sections would not work
! in array initializers. Use of implied do loop variables for indices
! and substrings, with and without implied do loops, were fixed at the
! same time.
!
! Contributed by Paul Thomas   <pault@gcc.gnu.org>
! based on testcase from Harald Anlauf  <anlauf@gmx.de>  
!
  real, parameter :: x(4,4) = reshape((/(i, i = 1, 16)/), (/4,4/))
  real, parameter :: y(4) = (/ x(1:2, 2), x(3:4, 4)/)
  real, parameter :: z(2) = x(2:3, 3) + 1
  real, parameter :: r(6) = (/(x(i:i +1, i), i = 1,3)/)
  real, parameter :: s(12) = (/((x(i, i:j-1:-1), i = 3,4), j = 2,3)/)
  real, parameter :: t(8) = (/(z, &
        real (i)**3, y(i), i = 2, 3)/) ! { dg-warning "nonstandard" }

  integer, parameter :: ii = 4

  character(4), parameter :: chr(4) = (/"abcd", "efgh", "ijkl", "mnop"/)
  character(4), parameter :: chrs = chr(ii)(2:3)//chr(2)(ii-3:ii-2) 
  character(4), parameter :: chrt(2) = (/chr(2:2)(2:3), chr(ii-1)(3:ii)/)
  character(2), parameter :: chrx(2) = (/(chr(i)(i:i+1), i=2,3)/)

  if (any (y .ne. (/5., 6., 15., 16./))) call abort ()
  if (any (z .ne. (/11., 12./))) call abort ()
  if (any (r .ne. (/1., 2., 6., 7., 11., 12./))) call abort ()
  if (any (s .ne. (/11., 7., 3., 16., 12., 8., 4., &
                    11., 7.,     16., 12., 8. /))) call abort ()

  if (any (t .ne. (/11., 12., 8., 6., 11., 12., 27., 15. /))) call abort ()

  if (chrs .ne. "noef") call abort ()
  if (any (chrt .ne. (/"fg", "kl"/))) call abort ()
  if (any (chrx .ne. (/"fg", "kl"/))) call abort ()
end
