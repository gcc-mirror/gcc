! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/61831
! The deallocation of components of array constructor elements
! used to have the side effect of also deallocating some other
! variable's components from which they were copied.

program main
  implicit none

  integer, parameter :: n = 2

  type :: string_t
     character(LEN=1), dimension(:), allocatable :: chars
  end type string_t

  type :: string_container_t
     type(string_t) :: comp
  end type string_container_t

  type :: string_array_container_t
     type(string_t) :: comp(n)
  end type string_array_container_t

  type(string_t) :: prt_in, tmp, tmpa(n)
  type(string_container_t) :: tmpc, tmpca(n)
  type(string_array_container_t) :: tmpac, tmpaca(n)
  integer :: i, j, k

  do i=1,16

     ! Test without intermediary function
     prt_in = string_t(["A"])
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "A")) call abort
     deallocate (prt_in%chars)

     ! scalar elemental function
     prt_in = string_t(["B"])
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "B")) call abort
     tmp = new_prt_spec (prt_in)
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "B")) call abort
     deallocate (prt_in%chars)
     deallocate (tmp%chars)

     ! array elemental function with array constructor
     prt_in = string_t(["C"])
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "C")) call abort
     tmpa = new_prt_spec ([(prt_in, i=1,2)])
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "C")) call abort
     deallocate (prt_in%chars)
     do j=1,n
        deallocate (tmpa(j)%chars)
     end do

     ! scalar elemental function with structure constructor
     prt_in = string_t(["D"])
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "D")) call abort
     tmpc = new_prt_spec2 (string_container_t(prt_in))
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "D")) call abort
     deallocate (prt_in%chars)
     deallocate(tmpc%comp%chars)

     ! array elemental function of an array constructor of structure constructors
     prt_in = string_t(["E"])
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "E")) call abort
     tmpca = new_prt_spec2 ([ (string_container_t(prt_in), i=1,2) ])
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "E")) call abort
     deallocate (prt_in%chars)
     do j=1,n
        deallocate (tmpca(j)%comp%chars)
     end do

     ! scalar elemental function with a structure constructor and a nested array constructor
     prt_in = string_t(["F"])
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "F")) call abort
     tmpac = new_prt_spec3 (string_array_container_t([ (prt_in, i=1,2) ]))
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "F")) call abort
     deallocate (prt_in%chars)
     do j=1,n
        deallocate (tmpac%comp(j)%chars)
     end do

     ! array elemental function with an array constructor nested inside
     ! a structure constructor nested inside  an array constructor
     prt_in = string_t(["G"])
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "G")) call abort
     tmpaca = new_prt_spec3 ([ (string_array_container_t([ (prt_in, i=1,2) ]), j=1,2) ])
     if (.not. allocated(prt_in%chars)) call abort
     if (any(prt_in%chars .ne. "G")) call abort
     deallocate (prt_in%chars)
     do j=1,n
        do k=1,n
           deallocate (tmpaca(j)%comp(k)%chars)
        end do
     end do

  end do

contains

  elemental function new_prt_spec (name) result (prt_spec)
    type(string_t), intent(in) :: name
    type(string_t) :: prt_spec
    prt_spec = name
  end function new_prt_spec

  elemental function new_prt_spec2 (name) result (prt_spec)
    type(string_container_t), intent(in) :: name
    type(string_container_t) :: prt_spec
    prt_spec = name
  end function new_prt_spec2

  elemental function new_prt_spec3 (name) result (prt_spec)
    type(string_array_container_t), intent(in) :: name
    type(string_array_container_t) :: prt_spec
    prt_spec = name
  end function new_prt_spec3
end program main
! { dg-final { scan-tree-dump-times "__builtin_malloc" 15 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free" 33 "original" } }
