! { dg-do compile }

! Make sure that pr103414 is fixed.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
function p ()
   type t(n)
      integer, kind :: n
      character(n) :: c = ''
   end type
   type(t(3)) :: x = t(z'1') ! { dg-error "Expected an initialization expression" }
end

function q ()
   type t(n)
      integer, kind :: n
      character(n) :: c = ''
   end type
   type(t(3)) :: x(1) = [t(z'1')] ! { dg-error "Syntax error in array constructor" }
end
