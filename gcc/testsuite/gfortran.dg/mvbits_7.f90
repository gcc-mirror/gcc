! { dg-do run }

! PR fortran/38883
! This ICE'd because the temporary-creation in the MVBITS call was wrong.

! Contributed by Paul Richard Thomas <paul.richard.thomas@gmail.com>

  type t
    integer  ::  I
    character(9)  :: chr
  end type
  type(t) :: x(4,3)
  type(t) :: y(4,3)
  x = reshape ([((t (i*j, "a"),i = 1,4), j=1,3)], [4,3])
  call foo (x)
  y = reshape ([((t (i*j*2, "a"),i = 1,4), j=1,3)], [4,3])
  call bar(y, 4, 3, 1, -1, -4, -3)
  if (any (x%i .ne. y%i)) STOP 1
contains
  SUBROUTINE foo (x)
    TYPE(t) x(4, 3)      ! No dependency at all
    CALL MVBITS (x%i, 0, 6, x%i, 8)
    x%i = x%i * 2
  END SUBROUTINE
  SUBROUTINE bar (x, NF4, NF3, NF1, MF1, MF4, MF3)
    TYPE(t) x(NF4, NF3)  ! Dependency through variable indices
    CALL MVBITS (x(NF4:NF1:MF1, NF1:NF3)%i, 1, &
                 6, x(-MF4:-MF1:-NF1, -MF1:-MF3)%i, 9)
  END SUBROUTINE
end

! { dg-prune-output "reading \[0-9\]+ bytes from a region" }
