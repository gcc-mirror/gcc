! { dg-do run }
! PR fortran/112772 - test absent OPTIONAL, ALLOCATABLE/POINTER class dummies

program main
  implicit none
  type t
  end type t
  call test_c_a ()
  call test_u_a ()
  call test_c_p ()
  call test_u_p ()
contains
  ! class, allocatable
  subroutine test_c_a (msg1)
    class(t), optional, allocatable :: msg1(:)
    if (present (msg1)) stop 1
    call assert_c_a ()
    call assert_c_a (msg1)
  end

  subroutine assert_c_a (msg2)
    class(t), optional, allocatable :: msg2(:)
    if (present (msg2)) stop 2
  end

  ! unlimited polymorphic, allocatable
  subroutine test_u_a (msg1)
    class(*), optional, allocatable :: msg1(:)
    if (present (msg1)) stop 3
    call assert_u_a ()
    call assert_u_a (msg1)
  end

  subroutine assert_u_a (msg2)
    class(*), optional, allocatable :: msg2(:)
    if (present (msg2)) stop 4
  end

  ! class, pointer
  subroutine test_c_p (msg1)
    class(t), optional, pointer :: msg1(:)
    if (present (msg1)) stop 5
    call assert_c_p ()
    call assert_c_p (msg1)
  end

  subroutine assert_c_p (msg2)
    class(t), optional, pointer :: msg2(:)
    if (present (msg2)) stop 6
  end

  ! unlimited polymorphic, pointer
  subroutine test_u_p (msg1)
    class(*), optional, pointer :: msg1(:)
    if (present (msg1)) stop 7
    call assert_u_p ()
    call assert_u_p (msg1)
  end

  subroutine assert_u_p (msg2)
    class(*), optional, pointer :: msg2(:)
    if (present (msg2)) stop 8
  end
end
