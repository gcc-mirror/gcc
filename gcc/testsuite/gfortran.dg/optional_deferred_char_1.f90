! { dg-do run }
! PR fortran/93762
! PR fortran/100651 - deferred-length character as optional dummy argument

program main
  implicit none
  character(:), allocatable :: err_msg, msg3(:)
  character(:), pointer     :: err_msg2 => NULL()

  ! Subroutines with optional arguments
  call to_int ()
  call to_int_p ()
  call test_rank1 ()
  call assert_code ()
  call assert_p ()
  call assert_rank1 ()

  ! Test passing of optional arguments
  call to_int (err_msg)
  if (.not. allocated (err_msg)) stop 1
  if (len (err_msg) /= 7)        stop 2
  if (err_msg(1:7) /= "foo bar") stop 3

  call to_int2 (err_msg)
  if (.not. allocated (err_msg)) stop 4
  if (len (err_msg) /= 7)        stop 5
  if (err_msg(1:7) /= "foo bar") stop 6
  deallocate (err_msg)

  call to_int_p (err_msg2)
  if (.not. associated (err_msg2)) stop 11
  if (len (err_msg2) /= 8)         stop 12
  if (err_msg2(1:8) /= "poo bla ") stop 13
  deallocate (err_msg2)

  call to_int2_p (err_msg2)
  if (.not. associated (err_msg2)) stop 14
  if (len (err_msg2) /= 8)         stop 15
  if (err_msg2(1:8) /= "poo bla ") stop 16
  deallocate (err_msg2)

  call test_rank1 (msg3)
  if (.not. allocated (msg3)) stop 21
  if (len (msg3) /= 2)        stop 22
  if (size (msg3) /= 42)      stop 23
  if (any (msg3 /= "ok"))     stop 24
  deallocate (msg3)

contains

  ! Deferred-length character, allocatable:
  subroutine assert_code (err_msg0)
    character(:), optional, allocatable :: err_msg0
    if (present (err_msg0)) err_msg0 = 'foo bar'
  end
  ! Test: optional argument
  subroutine to_int (err_msg1)
    character(:), optional, allocatable :: err_msg1
    call assert_code (err_msg1)
  end
  ! Control: non-optional argument
  subroutine to_int2 (err_msg2)
    character(:), allocatable :: err_msg2
    call assert_code (err_msg2)
  end

  ! Rank-1:
  subroutine assert_rank1 (msg)
    character(:), optional, allocatable, intent(out) :: msg(:)
    if (present (msg)) then
       allocate (character(2) :: msg(42))
       msg(:) = "ok"
    end if
  end

  subroutine test_rank1 (msg1)
    character(:), optional, allocatable, intent(out) :: msg1(:)
    call assert_rank1 (msg1)
  end

  ! Deferred-length character, pointer:
  subroutine assert_p (err_msg0)
    character(:), optional, pointer :: err_msg0
    if (present (err_msg0)) then
       if (associated (err_msg0)) deallocate (err_msg0)
       allocate (character(8) :: err_msg0)
       err_msg0 = 'poo bla'
    end if
  end

  subroutine to_int_p (err_msg1)
    character(:), optional, pointer :: err_msg1
    call assert_p (err_msg1)
  end

  subroutine to_int2_p (err_msg2)
    character(:), pointer :: err_msg2
    call assert_p (err_msg2)
  end
end
