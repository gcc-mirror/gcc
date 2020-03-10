! { dg-do run }
! { dg-options "-fcheck=all" }
!
! Test the fix for PR93581 and the implementation of note 9.7 of F2018.
! The latter requires that the result of the LEN inquiry be a scalar
! even for array expressions.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   complex, target :: z(2) = [(1.0, 2.0),(3.0, 4.0)]
   character(:), allocatable, target :: c(:)
   real, pointer :: r(:)
   character(:), pointer :: s(:)

   r => z%re
   if (any (r .ne. real (z))) stop 1
   r => z%im
   if (any (r .ne. imag (z))) stop 2

   allocate (c, source = ['abc','def'])
   s(-2:-1) => c(1:2)
   if (s%len .ne. len (c)) stop 3
end
