! { dg-do compile } 
! { dg-additional-options "-fmax-errors=100" } 

module test 
  implicit none
contains

  subroutine foo (vi)
  logical :: l
  integer, value :: vi
  integer :: i, ia(10), a(10), b(2:8)
  complex :: c, ca(10)
  real, target:: r
  real :: ra(10)
  real, pointer :: rp
  real, dimension(:), allocatable :: aa
  type t
  integer :: i
  end type
  type(t) :: ti
  type(t), allocatable :: tia
  type(t), target :: tit
  type(t), pointer :: tip
  rp => r
  tip => tit

  ! enter data
  !$acc enter data
  !$acc enter data if (.false.)
  !$acc enter data if (l)
  !$acc enter data if (.false.) if (l) ! { dg-error "Failed to match clause" }
  !$acc enter data if (i) ! { dg-error "LOGICAL" }
  !$acc enter data if (1) ! { dg-error "LOGICAL" }
  !$acc enter data if (a) ! { dg-error "LOGICAL" }
  !$acc enter data if (b(5:6)) ! { dg-error "LOGICAL" }
  !$acc enter data async (l) ! { dg-error "INTEGER" }
  !$acc enter data async (.true.) ! { dg-error "INTEGER" }
  !$acc enter data async (1) 
  !$acc enter data async (i) 
  !$acc enter data async (a) ! { dg-error "INTEGER" }
  !$acc enter data async (b(5:6)) ! { dg-error "INTEGER" }
  !$acc enter data wait (l) ! { dg-error "INTEGER" }
  !$acc enter data wait (.true.) ! { dg-error "INTEGER" }
  !$acc enter data wait (i, 1) 
  !$acc enter data wait (a) ! { dg-error "INTEGER" }
  !$acc enter data wait (b(5:6)) ! { dg-error "INTEGER" }
  !$acc enter data copyin (tip)
  !$acc enter data copyin (tia)
  !$acc enter data create (tip)
  !$acc enter data create (tia)
  !$acc enter data present_or_copyin (tip)
  !$acc enter data present_or_copyin (tia)
  !$acc enter data present_or_create (tip)
  !$acc enter data present_or_create (tia)
  !$acc enter data copyin (i) create (i) ! { dg-error "multiple clauses" }
  !$acc enter data copyin (i) present_or_copyin (i) ! { dg-error "multiple clauses" }
  !$acc enter data create (i) present_or_copyin (i) ! { dg-error "multiple clauses" }
  !$acc enter data copyin (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc enter data create (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc enter data present_or_copyin (i) present_or_create (i) ! { dg-error "multiple clauses" }

  ! exit data
  !$acc exit data
  !$acc exit data if (.false.)
  !$acc exit data if (l)
  !$acc exit data if (.false.) if (l) ! { dg-error "Failed to match clause" }
  !$acc exit data if (i) ! { dg-error "LOGICAL" }
  !$acc exit data if (1) ! { dg-error "LOGICAL" }
  !$acc exit data if (a) ! { dg-error "LOGICAL" }
  !$acc exit data if (b(5:6)) ! { dg-error "LOGICAL" }
  !$acc exit data async (l) ! { dg-error "INTEGER" }
  !$acc exit data async (.true.) ! { dg-error "INTEGER" }
  !$acc exit data async (1) 
  !$acc exit data async (i) 
  !$acc exit data async (a) ! { dg-error "INTEGER" }
  !$acc exit data async (b(5:6)) ! { dg-error "INTEGER" }
  !$acc exit data wait (l) ! { dg-error "INTEGER" }
  !$acc exit data wait (.true.) ! { dg-error "INTEGER" }
  !$acc exit data wait (i, 1) 
  !$acc exit data wait (a) ! { dg-error "INTEGER" }
  !$acc exit data wait (b(5:6)) ! { dg-error "INTEGER" }
  !$acc exit data copyout (tip)
  !$acc exit data copyout (tia)
  !$acc exit data delete (tip)
  !$acc exit data delete (tia)
  !$acc exit data copyout (i) delete (i) ! { dg-error "multiple clauses" }
  !$acc exit data finalize
  !$acc exit data finalize copyout (i)
  !$acc exit data finalize delete (i)
  end subroutine foo
end module test
