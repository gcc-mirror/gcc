! { dg-do compile }
!
! The gimplifier used to throw a fit on thes two functions.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
function f1(x)
   character(*) :: x(*)
   print *, g(x%len)
end

function f2(x)
   character(*) :: x(3)
   print *, g(x%len)
end
