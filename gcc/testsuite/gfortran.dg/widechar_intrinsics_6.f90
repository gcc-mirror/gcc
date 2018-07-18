! { dg-do run }
! { dg-options "-fbackslash" }

  character(kind=1, len=3) :: s1
  character(kind=4, len=3) :: s4
  integer :: i

  s1 = "fo "
  s4 = 4_"fo "
  i = 3

  ! Check the REPEAT intrinsic

  if (repeat (1_"foo", 2) /= 1_"foofoo") STOP 1
  if (repeat (1_"fo ", 2) /= 1_"fo fo ") STOP 2
  if (repeat (1_"fo ", 2) /= 1_"fo fo") STOP 3
  if (repeat (1_"fo ", 0) /= 1_"") STOP 4
  if (repeat (s1, 2) /= 1_"fo fo ") STOP 5
  if (repeat (s1, 2) /= 1_"fo fo") STOP 6
  if (repeat (s1, 2) /= s1 // s1) STOP 7
  if (repeat (s1, 3) /= s1 // s1 // s1) STOP 8
  if (repeat (s1, 1) /= s1) STOP 9
  if (repeat (s1, 0) /= "") STOP 10

  if (repeat (4_"foo", 2) /= 4_"foofoo") STOP 11
  if (repeat (4_"fo ", 2) /= 4_"fo fo ") STOP 12
  if (repeat (4_"fo ", 2) /= 4_"fo fo") STOP 13
  if (repeat (4_"fo ", 0) /= 4_"") STOP 14
  if (repeat (s4, 2) /= 4_"fo fo ") STOP 15
  if (repeat (s4, 2) /= 4_"fo fo") STOP 16
  if (repeat (s4, 3) /= s4 // s4 // s4) STOP 17
  if (repeat (s4, 1) /= s4) STOP 18
  if (repeat (s4, 0) /= 4_"") STOP 19

  call check_repeat (s1, s4)
  call check_repeat ("", 4_"")
  call check_repeat ("truc", 4_"truc")
  call check_repeat ("truc ", 4_"truc ")

  ! Check NEW_LINE

  if (ichar(new_line ("")) /= 10) STOP 20
  if (len(new_line ("")) /= 1) STOP 21
  if (ichar(new_line (s1)) /= 10) STOP 22
  if (len(new_line (s1)) /= 1) STOP 23
  if (ichar(new_line (["",""])) /= 10) STOP 24
  if (len(new_line (["",""])) /= 1) STOP 25
  if (ichar(new_line ([s1,s1])) /= 10) STOP 26
  if (len(new_line ([s1,s1])) /= 1) STOP 27

  if (ichar(new_line (4_"")) /= 10) STOP 28
  if (len(new_line (4_"")) /= 1) STOP 29
  if (ichar(new_line (s4)) /= 10) STOP 30
  if (len(new_line (s4)) /= 1) STOP 31
  if (ichar(new_line ([4_"",4_""])) /= 10) STOP 32
  if (len(new_line ([4_"",4_""])) /= 1) STOP 33
  if (ichar(new_line ([s4,s4])) /= 10) STOP 34
  if (len(new_line ([s4,s4])) /= 1) STOP 35

  ! Check SIZEOF

  if (sizeof ("") /= 0) STOP 36
  if (sizeof (4_"") /= 0) STOP 37
  if (sizeof ("x") /= 1) STOP 38
  if (sizeof ("\xFF") /= 1) STOP 39
  if (sizeof (4_"x") /= 4) STOP 40
  if (sizeof (4_"\UFFFFFFFF") /= 4) STOP 41
  if (sizeof (s1) /= 3) STOP 42
  if (sizeof (s4) /= 12) STOP 43

  if (sizeof (["a", "x", "z"]) / sizeof ("a") /= 3) STOP 44
  if (sizeof ([4_"a", 4_"x", 4_"z"]) / sizeof (4_"a") /= 3) STOP 45

  call check_sizeof ("", 4_"", 0)
  call check_sizeof ("x", 4_"x", 1)
  call check_sizeof ("\xFF", 4_"\UFEBCE19E", 1)
  call check_sizeof ("\xFF ", 4_"\UFEBCE19E ", 2)
  call check_sizeof (s1, s4, 3)

contains

  subroutine check_repeat (s1, s4)
    character(kind=1, len=*), intent(in) :: s1
    character(kind=4, len=*), intent(in) :: s4
    integer :: i

    do i = 0, 10
      if (len (repeat(s1, i)) /= i * len(s1)) STOP 46
      if (len (repeat(s4, i)) /= i * len(s4)) STOP 47

      if (len_trim (repeat(s1, i)) &
          /= max(0, (i - 1) * len(s1) + len_trim (s1))) STOP 48
      if (len_trim (repeat(s4, i)) &
          /= max(0, (i - 1) * len(s4) + len_trim (s4))) STOP 49
    end do
  end subroutine check_repeat

  subroutine check_sizeof (s1, s4, i)
    character(kind=1, len=*), intent(in) :: s1
    character(kind=4, len=*), intent(in) :: s4
    character(kind=4, len=len(s4)) :: t4
    integer, intent(in) :: i
    
    if (sizeof (s1) /= i) STOP 50
    if (sizeof (s4) / sizeof (4_" ") /= i) STOP 51
    if (sizeof (t4) / sizeof (4_" ") /= i) STOP 52
  end subroutine check_sizeof

end
