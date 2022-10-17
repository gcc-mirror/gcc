! PR 101309
! { dg-do run }
! { dg-additional-sources "fc-descriptor-7-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests passing arrays that may not be contiguous through
! descriptors to C functions as assumed-shape arguments.

program testit
  use iso_c_binding
  implicit none (type, external)

  interface
    subroutine ctest (a, is_cont) bind (c)
      use iso_c_binding
      integer(C_INT) :: a(:,:)
      logical(C_Bool), value :: is_cont
    end subroutine
    subroutine ctest_cont (a, is_cont) bind (c, name="ctest")
      use iso_c_binding
      integer(C_INT), contiguous :: a(:,:)
      logical(C_Bool), value :: is_cont
    end subroutine

    subroutine ctest_ar (a, is_cont) bind (c, name="ctest")
      use iso_c_binding
      integer(C_INT) :: a(..)
      logical(C_Bool), value :: is_cont
    end subroutine
    subroutine ctest_ar_cont (a, is_cont) bind (c, name="ctest")
      use iso_c_binding
      integer(C_INT), contiguous :: a(..)
      logical(C_Bool), value :: is_cont
    end subroutine
  end interface

  integer :: i , j
  integer(C_INT), target :: aa(10,5)
  integer(C_INT), target :: bb(10,10)

  ! Original array
  do j = 1, 5
    do i = 1, 10
      aa(i,j) = i + 100*j
    end do
  end do

  ! Transposed array
  do j = 2, 10, 2 
    do i = 1, 10
      bb(j, i) = i + 100*((j-2)/2 + 1)
    end do
  end do
 
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1

  ! Test both calling the C function directly, and via another function
  ! that takes an assumed-shape/assumed-rank argument.

  call ftest (transpose (aa), is_cont=.true._c_bool) ! Implementation choice: copy in; hence, contiguous
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1

  call ctest (transpose (aa), is_cont=.false._c_bool)  ! Implementation choice: noncontigous / sm inversed
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1
  call ctest_cont (transpose (aa), is_cont=.true._c_bool)
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1
  call ctest_ar (transpose (aa), is_cont=.false._c_bool)  ! Implementation choice: noncontigous / sm inversed
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1
  call ctest_ar_cont (transpose (aa), is_cont=.true._c_bool)
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1


  call ftest (bb(2:10:2, :), is_cont=.false._c_bool)
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1

  call ctest (bb(2:10:2, :), is_cont=.false._c_bool)
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1
  call ctest_cont (bb(2:10:2, :), is_cont=.true._c_bool)
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1
  call ctest_ar (bb(2:10:2, :), is_cont=.false._c_bool)
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1
  call ctest_ar_cont (bb(2:10:2, :), is_cont=.true._c_bool)
  if (any (transpose (aa) /= bb(2:10:2, :))) error stop 1

contains
  subroutine ftest (a, is_cont)
    use iso_c_binding
    integer(C_INT) :: a(:,:)
    logical(c_bool), value, intent(in) :: is_cont
    if (is_cont .NEQV. is_contiguous (a)) error stop 2
    if (any (shape (a) /= [5, 10])) error stop 3
    do j = 1, 5
      do i = 1, 10
        if (a(j, i) /= i + 100*j) error stop 4
        if (a(j, i) /= aa(i,j)) error stop 
      end do
    end do
    call ctest (a, is_cont)
    call ctest_cont (a, is_cont=.true._c_bool)
    call ctest_ar (a, is_cont)
    call ctest_ar_cont (a, is_cont=.true._c_bool)
  end subroutine

  subroutine ftest_ar (a, is_cont)
    use iso_c_binding
    integer(C_INT) :: a(..)
    logical(c_bool), value, intent(in) :: is_cont
    if (is_cont .NEQV. is_contiguous (a)) error stop 2
    if (any (shape (a) /= [5, 10])) error stop 3
    select rank (a)
    rank(2)
      do j = 1, 5
        do i = 1, 10
          if (a(j, i) /= i + 100*j) error stop 4
          if (a(j, i) /= aa(i,j)) error stop 
        end do
      end do
      call ctest (a, is_cont)
      call ctest_cont (a, is_cont=.true._c_bool)
      call ftest_ar_con (a, is_cont=.true._c_bool)
    end select
    call ctest_ar (a, is_cont)
    ! call ctest_ar_cont (a, is_cont=.true._c_bool)  ! TODO/FIXME: ICE, cf. PR fortran/102729
    ! call ftest_ar_con (a, is_cont=.true._c_bool)   ! TODO/FIXME: ICE, cf. PR fortran/102729
  end subroutine

  subroutine ftest_ar_con (a, is_cont)
    use iso_c_binding
    integer(C_INT), contiguous :: a(..)
    logical(c_bool), value, intent(in) :: is_cont
    if (is_cont .NEQV. is_contiguous (a)) error stop 2
    if (any (shape (a) /= [5, 10])) error stop 3
    select rank (a)
    rank(2)
      do j = 1, 5
        do i = 1, 10
          if (a(j, i) /= i + 100*j) error stop 4
          if (a(j, i) /= aa(i,j)) error stop 
        end do
      end do
      call ctest (a, is_cont)
      call ctest_cont (a, is_cont=.true._c_bool)
    end select
    call ctest_ar (a, is_cont)
    call ctest_ar_cont (a, is_cont=.true._c_bool)
  end subroutine
end program
