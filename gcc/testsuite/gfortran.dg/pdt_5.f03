! { dg-do run }
!
! Third, complete example from the PGInsider article:
! "Object-Oriented Programming in Fortran 2003 Part 3: Parameterized Derived Types"
! by Mark Leair
!
!     Copyright (c) 2013, NVIDIA CORPORATION.  All rights reserved.
!
! NVIDIA CORPORATION and its licensors retain all intellectual property
! and proprietary rights in and to this software, related documentation
! and any modifications thereto.  Any use, reproduction, disclosure or
! distribution of this software and related documentation without an express
! license agreement from NVIDIA CORPORATION is strictly prohibited.
!

!          THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT
!   WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT
!   NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR
!   FITNESS FOR A PARTICULAR PURPOSE.
!
! Note that modification had to be made all of which are commented.
!
module matrix

type :: base_matrix(k,c,r)
  private
    integer, kind :: k = 4
    integer, len :: c = 1
    integer, len :: r = 1
end type base_matrix

type, extends(base_matrix) ::  adj_matrix
  private
    class(*), pointer :: m(:,:) => null()
end type adj_matrix

interface getKind
  module procedure getKind4
  module procedure getKind8
end interface getKind

interface getColumns
  module procedure getNumCols4
  module procedure getNumCols8
end interface getColumns

interface getRows
  module procedure getNumRows4
  module procedure getNumRows8
end interface getRows

interface adj_matrix
   module procedure construct_4   ! kind=4 constructor
   module procedure construct_8   ! kind=8 constructor
end interface adj_matrix

interface assignment(=)
   module procedure m2m4          ! assign kind=4 matrix
   module procedure a2m4          ! assign kind=4 array
   module procedure m2m8          ! assign kind=8 matrix
   module procedure a2m8          ! assign kind=8 array
   module procedure m2a4          ! assign kind=4 matrix to array
   module procedure m2a8          ! assign kind=8 matrix to array
end interface assignment(=)


contains

  function getKind4(this) result(rslt)
   class(adj_matrix(4,*,*)) :: this
   integer :: rslt
   rslt = this%k
  end function getKind4

 function getKind8(this) result(rslt)
   class(adj_matrix(8,*,*)) :: this
   integer :: rslt
   rslt = this%k
 end function getKind8

  function getNumCols4(this) result(rslt)
   class(adj_matrix(4,*,*)) :: this
   integer :: rslt
   rslt = this%c
  end function getNumCols4

  function getNumCols8(this) result(rslt)
   class(adj_matrix(8,*,*)) :: this
   integer :: rslt
   rslt = this%c
  end function getNumCols8

  function getNumRows4(this) result(rslt)
   class(adj_matrix(4,*,*)) :: this
   integer :: rslt
   rslt = this%r
  end function getNumRows4

  function getNumRows8(this) result(rslt)
   class(adj_matrix(8,*,*)) :: this
   integer :: rslt
   rslt = this%r
  end function getNumRows8


 function construct_4(k,c,r) result(mat)
     integer(4) :: k
     integer :: c
     integer :: r
     class(adj_matrix(4,:,:)),allocatable :: mat

     allocate(adj_matrix(4,c,r)::mat)

  end function construct_4

  function construct_8(k,c,r) result(mat)
     integer(8) :: k
     integer :: c
     integer :: r
     class(adj_matrix(8,:,:)),allocatable :: mat

     allocate(adj_matrix(8,c,r)::mat)

  end function construct_8

  subroutine a2m4(d,s)
   class(adj_matrix(4,:,:)),allocatable :: d
   class(*),dimension(:,:) :: s

   if (allocated(d)) deallocate(d)
!    allocate(adj_matrix(4,size(s,1),size(s,2))::d)     ! generates assembler error
   allocate(d, mold = adj_matrix(4,size(s,1),size(s,2)))
   allocate(d%m(size(s,1),size(s,2)),source=s)
 end subroutine a2m4

 subroutine a2m8(d,s)
   class(adj_matrix(8,:,:)),allocatable :: d
   class(*),dimension(:,:) :: s

   if (allocated(d)) deallocate(d)
!    allocate(adj_matrix(8,size(s,1),size(s,2))::d)     ! generates assembler error
   allocate(d, mold = adj_matrix(8_8,size(s,1),size(s,2))) ! Needs 8_8 to match arg1 of 'construct_8'
   allocate(d%m(size(s,1),size(s,2)),source=s)
 end subroutine a2m8

subroutine m2a8(a,this)
class(adj_matrix(8,*,*)), intent(in) :: this         ! Intents required for
real(8),allocatable, intent(out) :: a(:,:)           ! defined assignment
  select type (array => this%m)                      ! Added SELECT TYPE because...
    type is (real(8))
  if (allocated(a)) deallocate(a)
  allocate(a,source=array)
  end select
!   allocate(a(size(this%m,1),size(this%m,2)),source=this%m) ! ...CLASS(*) source does not work in gfortran
 end subroutine m2a8

 subroutine m2a4(a,this)
 class(adj_matrix(4,*,*)), intent(in) :: this        ! Intents required for
 real(4),allocatable, intent(out) :: a(:,:)          ! defined assignment
  select type (array => this%m)                      ! Added SELECT TYPE because...
    type is (real(4))
   if (allocated(a)) deallocate(a)
   allocate(a,source=array)
  end select
!   allocate(a(size(this%m,1),size(this%m,2)),source=this%m) ! ...CLASS(*) source does not work in gfortran
 end subroutine m2a4

  subroutine m2m4(d,s)
   CLASS(adj_matrix(4,:,:)),allocatable, intent(OUT) :: d   ! Intents required for
   CLASS(adj_matrix(4,*,*)), intent(in) :: s                ! defined assignment

   if (allocated(d)) deallocate(d)
   allocate(d,source=s)
 end subroutine m2m4

 subroutine m2m8(d,s)
   CLASS(adj_matrix(8,:,:)),allocatable, intent(OUT) :: d   ! Intents required for
   CLASS(adj_matrix(8,*,*)), intent(in) :: s                ! defined assignment

   if (allocated(d)) deallocate(d)
   allocate(d,source=s)
 end subroutine m2m8


end module matrix


program adj3

  use matrix
  implicit none
  integer(8) :: i

  class(adj_matrix(8,:,:)),allocatable :: adj             ! Was TYPE: Fails in
  real(8) :: a(2,3)                                       ! defined assignment
  real(8),allocatable :: b(:,:)

  class(adj_matrix(4,:,:)),allocatable :: adj_4           ! Ditto and ....
  real(4) :: a_4(3,2)                                     ! ... these declarations were
  real(4),allocatable :: b_4(:,:)                         ! added to check KIND=4

! Check constructor of PDT and instrinsic assignment
  adj = adj_matrix(INT(8,8),2,4)
  if (adj%k .ne. 8) STOP 1
  if (adj%c .ne. 2) STOP 2
  if (adj%r .ne. 4) STOP 3
  a = reshape ([(i, i = 1, 6)], [2,3])
  adj = a
  b = adj
  if (any (b .ne. a)) STOP 4

! Check allocation with MOLD of PDT. Note that only KIND parameters set.
  allocate (adj_4, mold = adj_matrix(4,3,2))           ! Added check of KIND = 4
  if (adj_4%k .ne. 4) STOP 5
  a_4 = reshape (a, [3,2])
  adj_4 = a_4
  b_4 = adj_4
  if (any (b_4 .ne. a_4)) STOP 6

end program adj3



