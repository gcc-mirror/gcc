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
  if (adjustl (s4) /= adjustl (4_"\0  foo bar \xFF        ")) call abort
  if (adjustr (s4) /= adjustr (4_"\0  foo bar \xFF        ")) call abort

  s4 = "   \0  foo bar \xFF"
  if (adjustl (s4) /= adjustl (4_"   \0  foo bar \xFF     ")) call abort
  if (adjustr (s4) /= adjustr (4_"   \0  foo bar \xFF     ")) call abort

  s4 = 4_" \U12345678\xeD bar \ufd30"
  if (adjustl (s4) /= &
      adjustl (4_" \U12345678\xeD bar \ufd30           ")) call abort
  if (adjustr (s4) /= &
      adjustr (4_" \U12345678\xeD bar \ufd30           ")) call abort

contains

  subroutine test_adjust1 (s1, s4)

    character(kind=1,len=*) :: s1
    character(kind=4,len=*) :: s4

    character(kind=1,len=len(s4)) :: t1
    character(kind=4,len=len(s1)) :: t4

    if (len(s1) /= len(s4)) call abort
    if (len(t1) /= len(t4)) call abort

    if (len_trim(s1) /= len_trim (s4)) call abort

    t1 = adjustl (s4)
    t4 = adjustl (s1)
    if (t1 /= adjustl (s1)) call abort
    if (t4 /= adjustl (s4)) call abort
    if (len_trim (t1) /= len_trim (t4)) call abort
    if (len_trim (adjustl (s1)) /= len_trim (t4)) call abort
    if (len_trim (adjustl (s4)) /= len_trim (t1)) call abort

    if (len_trim (t1) /= len (trim (t1))) call abort
    if (len_trim (s1) /= len (trim (s1))) call abort
    if (len_trim (t4) /= len (trim (t4))) call abort
    if (len_trim (s4) /= len (trim (s4))) call abort

    t1 = adjustr (s4)
    t4 = adjustr (s1)
    if (t1 /= adjustr (s1)) call abort
    if (t4 /= adjustr (s4)) call abort
    if (len_trim (t1) /= len_trim (t4)) call abort
    if (len_trim (adjustr (s1)) /= len_trim (t4)) call abort
    if (len_trim (adjustr (s4)) /= len_trim (t1)) call abort
    if (len (t1) /= len_trim (t1)) call abort
    if (len (t4) /= len_trim (t4)) call abort

    if (len_trim (t1) /= len (trim (t1))) call abort
    if (len_trim (s1) /= len (trim (s1))) call abort
    if (len_trim (t4) /= len (trim (t4))) call abort
    if (len_trim (s4) /= len (trim (s4))) call abort

  end subroutine test_adjust1

  subroutine test_adjust2 (s1, s4)

    character(kind=1,len=20) :: s1
    character(kind=4,len=20) :: s4

    character(kind=1,len=len(s4)) :: t1
    character(kind=4,len=len(s1)) :: t4

    if (len(s1) /= len(s4)) call abort
    if (len(t1) /= len(t4)) call abort

    if (len_trim(s1) /= len_trim (s4)) call abort

    t1 = adjustl (s4)
    t4 = adjustl (s1)
    if (t1 /= adjustl (s1)) call abort
    if (t4 /= adjustl (s4)) call abort
    if (len_trim (t1) /= len_trim (t4)) call abort
    if (len_trim (adjustl (s1)) /= len_trim (t4)) call abort
    if (len_trim (adjustl (s4)) /= len_trim (t1)) call abort

    if (len_trim (t1) /= len (trim (t1))) call abort
    if (len_trim (s1) /= len (trim (s1))) call abort
    if (len_trim (t4) /= len (trim (t4))) call abort
    if (len_trim (s4) /= len (trim (s4))) call abort

    t1 = adjustr (s4)
    t4 = adjustr (s1)
    if (t1 /= adjustr (s1)) call abort
    if (t4 /= adjustr (s4)) call abort
    if (len_trim (t1) /= len_trim (t4)) call abort
    if (len_trim (adjustr (s1)) /= len_trim (t4)) call abort
    if (len_trim (adjustr (s4)) /= len_trim (t1)) call abort
    if (len (t1) /= len_trim (t1)) call abort
    if (len (t4) /= len_trim (t4)) call abort

    if (len_trim (t1) /= len (trim (t1))) call abort
    if (len_trim (s1) /= len (trim (s1))) call abort
    if (len_trim (t4) /= len (trim (t4))) call abort
    if (len_trim (s4) /= len (trim (s4))) call abort

  end subroutine test_adjust2

end
