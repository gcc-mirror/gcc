! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }

! In this program shall be no kind=1,
! except for the 'argv' of the 'main' program.

! PR fortran/107266

! { dg-final { scan-tree-dump-times "kind=1" 1 "original" } }
! { dg-final { scan-tree-dump-times "character\\(kind=1\\) \\* \\* argv\\)" 1 "original" } }


! { dg-final { scan-tree-dump-times "character\\(kind=4\\) f \\(character\\(kind=4\\) x\\)" 1 "original" } }

character(kind=4) function f(x) bind(C)
  character(kind=4), value :: x
end

program testit
  implicit none (type, external)
  character (kind=4, len=:), allocatable :: aa
  character (kind=4, len=:), pointer :: pp

  pp => NULL ()

  call frobf (aa, pp)
  if (.not. allocated (aa)) stop 101
  if (storage_size(aa) /= storage_size(4_'foo')) stop 1
  if (aa .ne. 4_'foo') stop 102
  if (.not. associated (pp)) stop 103
  if (storage_size(pp) /= storage_size(4_'bar')) stop 2
  if (pp .ne. 4_'bar') stop 104

  pp => NULL ()

  call frobc (aa, pp)
  if (.not. allocated (aa)) stop 105
  if (storage_size(aa) /= storage_size(4_'frog')) stop 3
  if (aa .ne. 4_'frog') stop 106
  if (.not. associated (pp)) stop 107
  if (storage_size(pp) /= storage_size(4_'toad')) stop 4
  if (pp .ne. 4_'toad') stop 108


  contains

    subroutine frobf (a, p) Bind(C)
      character (kind=4, len=:), allocatable :: a
      character (kind=4, len=:), pointer :: p
      allocate (character(kind=4, len=3) :: p)
      a = 4_'foo'
      p = 4_'bar'
    end subroutine

    subroutine frobc (a, p) Bind(C)
      character (kind=4, len=:), allocatable :: a
      character (kind=4, len=:), pointer :: p
      allocate (character(kind=4, len=4) :: p)
      a = 4_'frog'
      p = 4_'toad'
    end subroutine

end program
