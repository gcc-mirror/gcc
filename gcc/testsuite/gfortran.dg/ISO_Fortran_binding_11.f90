! { dg-do run { target c99_runtime } }
! { dg-additional-sources ISO_Fortran_binding_11.c }
!
! Test the fix of PR89846.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
module mod_subobj_01
  use, intrinsic :: iso_c_binding
  implicit none
  integer, parameter :: nelem = 5
  type, bind(c) :: t1
     character(c_char) :: n
     real(c_float) :: r(2)
  end type t1
  type, bind(c) :: t2
     integer(c_long) :: i
     type(t1) :: t1
  end type t2
  interface
     subroutine ti(this, flag) bind(c)
       import :: t2, c_int
       type(t2) :: this(:)
       integer(c_int), value :: flag
     end subroutine ti
  end interface
contains
  subroutine ta0(this) bind(c)
    type(t1) :: this(:)
    integer :: i, iw, status
    status = 0
    if (size(this) /= nelem) then
       write(*,*) 'FAIL 1: ',size(this)
       status = status + 1
    end if
    iw = 0
    do i=1, nelem
       if (this(i)%n /= char(i,c_char) .or. this(i)%r(1) /= real(i,c_float) .or. &
            this(i)%r(2) /= real(i+1,c_float)) then
          iw = iw + 1
       end if
    end do
    if (iw > 0) then
       write(*,*) 'FAIL 2: ' ,this
       status = status + 1
    end if
    if (status /= 0) stop 1
  end subroutine ta0
  subroutine ta1(this) bind(c)
    integer(c_long) :: this(:)
    integer :: i, status
    status = 0
    if (size(this) /= nelem) then
       write(*,*) 'FAIL 3: ',size(this)
       status = status + 1
    end if
    if (maxval(abs(this - [ (int(i,c_long),i=1,nelem) ])) > 0) then
       write(*,*) 'FAIL 4: ' ,this
       status = status + 1
    end if
    if (status /= 0) stop 2
  end subroutine ta1
end module mod_subobj_01
program subobj_01
  use mod_subobj_01
  implicit none
  integer :: i

  type(t2), allocatable :: o_t2(:)

  allocate(o_t2(nelem))
  do i=1, nelem
     o_t2(i)%t1 = t1( char(i,c_char), [ real(i,c_float), real(i+1,c_float) ] )
     o_t2(i)%i = int(i,c_long)
  end do

  call ti(o_t2,0)
  call ti(o_t2,1)

end program subobj_01

