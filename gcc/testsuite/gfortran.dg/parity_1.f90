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

if (parity([logical ::]) .neqv. .false.) STOP 1
if (parity([.true., .false.]) .neqv. .true.) STOP 2
if (parity([.true.]) .neqv. .true.) STOP 3
if (parity([.false.]) .neqv. .false.) STOP 4
if (parity([.true., .false., .true.,.false.]) .neqv. .false.) STOP 5
if (parity(reshape([.true., .false., .true.,.false.],[2,2])) &
    .neqv. .false.) STOP 6
if (any (parity(reshape([.true., .false., .true.,.false.],[2,2]),dim=1) &
         .neqv. [.true., .true.])) STOP 7
if (any (parity(reshape([.true., .false., .true.,.false.],[2,2]),dim=2) &
         .neqv. [.false., .false.])) STOP 8

i = 0
if (parity(Lt(1:i)) .neqv. .false.) STOP 9
if (parity(Ltf) .neqv. .true.) STOP 10
if (parity(Lt) .neqv. .true.) STOP 11
if (parity(Lf) .neqv. .false.) STOP 12
if (parity(Ltftf) .neqv. .false.) STOP 13
if (parity(reshape(Ltftf,[2,2])) &
    .neqv. .false.) STOP 14
if (any (parity(reshape(Ltftf,[2,2]),dim=1) &
         .neqv. [.true., .true.])) STOP 15
if (any (parity(reshape(Ltftf,[2,2]),dim=2) &
         .neqv. [.false., .false.])) STOP 16

end
