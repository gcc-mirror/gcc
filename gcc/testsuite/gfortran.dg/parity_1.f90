! { dg-do run }
!
! PR fortran/33197
!
! Check implementation of PARITY
!
implicit none

integer :: i
logical :: Lt(1) = [ .true. ]
logical :: Lf(1) = [ .false.]
logical :: Ltf(2) = [ .true., .false. ]
logical :: Ltftf(4) = [.true., .false., .true.,.false.]

if (parity([logical ::]) .neqv. .false.) call abort()
if (parity([.true., .false.]) .neqv. .true.) call abort()
if (parity([.true.]) .neqv. .true.) call abort()
if (parity([.false.]) .neqv. .false.) call abort()
if (parity([.true., .false., .true.,.false.]) .neqv. .false.) call abort()
if (parity(reshape([.true., .false., .true.,.false.],[2,2])) &
    .neqv. .false.) call abort()
if (any (parity(reshape([.true., .false., .true.,.false.],[2,2]),dim=1) &
         .neqv. [.true., .true.])) call abort()
if (any (parity(reshape([.true., .false., .true.,.false.],[2,2]),dim=2) &
         .neqv. [.false., .false.])) call abort()

i = 0
if (parity(Lt(1:i)) .neqv. .false.) call abort()
if (parity(Ltf) .neqv. .true.) call abort()
if (parity(Lt) .neqv. .true.) call abort()
if (parity(Lf) .neqv. .false.) call abort()
if (parity(Ltftf) .neqv. .false.) call abort()
if (parity(reshape(Ltftf,[2,2])) &
    .neqv. .false.) call abort()
if (any (parity(reshape(Ltftf,[2,2]),dim=1) &
         .neqv. [.true., .true.])) call abort()
if (any (parity(reshape(Ltftf,[2,2]),dim=2) &
         .neqv. [.false., .false.])) call abort()

end
