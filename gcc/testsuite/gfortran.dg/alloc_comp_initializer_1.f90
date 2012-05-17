! { dg-do run }
! This checks the correct functioning of derived types with default initializers
! and allocatable components.
!
! Contributed by Salvatore Filippone  <salvatore.filippone@uniroma2.it>
!
module p_type_mod

  type m_type
    integer, allocatable :: p(:)
  end type m_type

  type basep_type
    type(m_type), allocatable :: av(:)
    type(m_type), pointer :: ap => null ()
    integer :: i = 101
  end type basep_type

  type p_type
    type(basep_type), allocatable :: basepv(:)
    integer :: p1 , p2 = 1
  end type p_type
end module p_type_mod

program foo
 
 use p_type_mod
  implicit none

  type(m_type), target :: a
  type(p_type) :: pre
  type(basep_type) :: wee

  call test_ab8 ()

  a = m_type ((/101,102/))  

  call p_bld (a, pre)

  if (associated (wee%ap) .or. wee%i /= 101) call abort ()
  wee%ap => a
  if (.not.associated (wee%ap) .or. allocated (wee%av)) call abort ()
  wee = basep_type ((/m_type ((/201, 202, 203/))/), null (), 99)
  if (.not.allocated (wee%av) .or. associated (wee%ap) .or. (wee%i .ne. 99)) call abort () 

contains

! Check that allocatable components are nullified after allocation.
  subroutine test_ab8 ()
    type(p_type)    :: p
    integer :: ierr
  
    if (.not.allocated(p%basepv)) then 
      allocate(p%basepv(1),stat=ierr)
    endif
    if (allocated (p%basepv) .neqv. .true.) call abort ()
    if (allocated (p%basepv(1)%av) .neqv. .false.) call abort
    if (p%basepv(1)%i .ne. 101) call abort ()

  end subroutine test_ab8

    subroutine p_bld (a, p)
      use p_type_mod
      type (m_type) :: a
      type(p_type) :: p
      if (any (a%p .ne. (/101,102/))) call abort ()
      if (allocated (p%basepv) .or. (p%p2 .ne. 1)) call abort ()
    end subroutine p_bld

end program foo
