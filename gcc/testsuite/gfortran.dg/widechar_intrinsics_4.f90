! { dg-do run }
! { dg-options "-fbackslash" }

  character(kind=1,len=20) :: s1
  character(kind=4,len=20) :: s4

  call test_adjust1 ("  foo bar ", 4_"  foo bar ")
  s1 = "  foo bar " ; s4 = 4_"  foo bar "
  call test_adjust2 (s1, s4)

  call test_adjust1 ("  foo bar \xFF", 4_"  foo bar \xFF")
  s1 = "  foo bar \xFF" ; s4 = 4_"  foo bar \xFF"
  call test_adjust2 (s1, s4)

  call test_adjust1 ("\0  foo bar \xFF", 4_"\0  foo bar \xFF")
  s1 = "\0  foo bar \xFF" ; s4 = 4_"\0  foo bar \xFF"
  call test_adjust2 (s1, s4)

  s4 = "\0  foo bar \xFF"
  if (adjustl (s4) /= adjustl (4_"\0  foo bar \xFF        ")) STOP 1
  if (adjustr (s4) /= adjustr (4_"\0  foo bar \xFF        ")) STOP 2

  s4 = "   \0  foo bar \xFF"
  if (adjustl (s4) /= adjustl (4_"   \0  foo bar \xFF     ")) STOP 3
  if (adjustr (s4) /= adjustr (4_"   \0  foo bar \xFF     ")) STOP 4

  s4 = 4_" \U12345678\xeD bar \ufd30"
  if (adjustl (s4) /= &
      adjustl (4_" \U12345678\xeD bar \ufd30           ")) STOP 5
  if (adjustr (s4) /= &
      adjustr (4_" \U12345678\xeD bar \ufd30           ")) STOP 6

contains

  subroutine test_adjust1 (s1, s4)

    character(kind=1,len=*) :: s1
    character(kind=4,len=*) :: s4

    character(kind=1,len=len(s4)) :: t1
    character(kind=4,len=len(s1)) :: t4

    if (len(s1) /= len(s4)) STOP 7
    if (len(t1) /= len(t4)) STOP 8

    if (len_trim(s1) /= len_trim (s4)) STOP 9

    t1 = adjustl (s4)
    t4 = adjustl (s1)
    if (t1 /= adjustl (s1)) STOP 10
    if (t4 /= adjustl (s4)) STOP 11
    if (len_trim (t1) /= len_trim (t4)) STOP 12
    if (len_trim (adjustl (s1)) /= len_trim (t4)) STOP 13
    if (len_trim (adjustl (s4)) /= len_trim (t1)) STOP 14

    if (len_trim (t1) /= len (trim (t1))) STOP 15
    if (len_trim (s1) /= len (trim (s1))) STOP 16
    if (len_trim (t4) /= len (trim (t4))) STOP 17
    if (len_trim (s4) /= len (trim (s4))) STOP 18

    t1 = adjustr (s4)
    t4 = adjustr (s1)
    if (t1 /= adjustr (s1)) STOP 19
    if (t4 /= adjustr (s4)) STOP 20
    if (len_trim (t1) /= len_trim (t4)) STOP 21
    if (len_trim (adjustr (s1)) /= len_trim (t4)) STOP 22
    if (len_trim (adjustr (s4)) /= len_trim (t1)) STOP 23
    if (len (t1) /= len_trim (t1)) STOP 24
    if (len (t4) /= len_trim (t4)) STOP 25

    if (len_trim (t1) /= len (trim (t1))) STOP 26
    if (len_trim (s1) /= len (trim (s1))) STOP 27
    if (len_trim (t4) /= len (trim (t4))) STOP 28
    if (len_trim (s4) /= len (trim (s4))) STOP 29

  end subroutine test_adjust1

  subroutine test_adjust2 (s1, s4)

    character(kind=1,len=20) :: s1
    character(kind=4,len=20) :: s4

    character(kind=1,len=len(s4)) :: t1
    character(kind=4,len=len(s1)) :: t4

    if (len(s1) /= len(s4)) STOP 30
    if (len(t1) /= len(t4)) STOP 31

    if (len_trim(s1) /= len_trim (s4)) STOP 32

    t1 = adjustl (s4)
    t4 = adjustl (s1)
    if (t1 /= adjustl (s1)) STOP 33
    if (t4 /= adjustl (s4)) STOP 34
    if (len_trim (t1) /= len_trim (t4)) STOP 35
    if (len_trim (adjustl (s1)) /= len_trim (t4)) STOP 36
    if (len_trim (adjustl (s4)) /= len_trim (t1)) STOP 37

    if (len_trim (t1) /= len (trim (t1))) STOP 38
    if (len_trim (s1) /= len (trim (s1))) STOP 39
    if (len_trim (t4) /= len (trim (t4))) STOP 40
    if (len_trim (s4) /= len (trim (s4))) STOP 41

    t1 = adjustr (s4)
    t4 = adjustr (s1)
    if (t1 /= adjustr (s1)) STOP 42
    if (t4 /= adjustr (s4)) STOP 43
    if (len_trim (t1) /= len_trim (t4)) STOP 44
    if (len_trim (adjustr (s1)) /= len_trim (t4)) STOP 45
    if (len_trim (adjustr (s4)) /= len_trim (t1)) STOP 46
    if (len (t1) /= len_trim (t1)) STOP 47
    if (len (t4) /= len_trim (t4)) STOP 48

    if (len_trim (t1) /= len (trim (t1))) STOP 49
    if (len_trim (s1) /= len (trim (s1))) STOP 50
    if (len_trim (t4) /= len (trim (t4))) STOP 51
    if (len_trim (s4) /= len (trim (s4))) STOP 52

  end subroutine test_adjust2

end
