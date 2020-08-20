! { dg-do compile }
!
! Test the fix for PRs 96100 and 96101.
!
! Contributed by Gerhardt Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      character(:), allocatable :: c(:)
   end type
   type(t) :: x
   character(:), allocatable :: w

! PR96100
   allocate(x%c(2), source = 'def')
   associate (y => [x%c(1:1)])       ! ICE
     print *,y
   end associate

! PR96101
   associate (y => ([w(:)]))
      print *, y                     ! ICE
   end associate

end
