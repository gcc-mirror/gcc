! { dg-do run }
! { dg-options "-O0 -fbounds-check" }
! Tests patch for PR29371, in which the null pointer
! assignment would cause a segfault with the bounds
! check on.
!
! Contributed by Tobias Burnus <tobias.burnus@physik.fu-berlin.de>
!
program test
  implicit none
  type projector_t
    real,   pointer :: ket(:, :), bra(:, :)
  end type projector_t

  type(projector_t),pointer, dimension(:) :: p
  integer :: stat,i
  allocate(p(2),stat=stat)
  do i = 1, 2
        nullify(p(i)%bra)
        nullify(p(i)%ket)
  end do
  do i = 1, 2
        if (associated (p(i)%bra)) call abort ()
        if (associated (p(i)%ket)) call abort ()
  end do
end program
