! { dg-do run }
! { dg-options "-fbackslash" }

  character(kind=1,len=20) :: s1
  character(kind=4,len=20) :: s4

  s1 = "this is me!"
  s4 = s1
  call check(s1, 4_"this is me!         ")
  call check2(s1, 4_"this is me!         ")
  s4 = "this is me!"
  call check(s1, 4_"this is me!         ")
  call check2(s1, 4_"this is me!         ")

  s1 = ""
  s4 = s1
  call check(s1, 4_"                    ")
  call check2(s1, 4_"                    ")
  s4 = ""
  call check(s1, 4_"                    ")
  call check2(s1, 4_"                    ")

  s1 = " \xFF"
  s4 = s1
  call check(s1, 4_" \xFF                  ")
  call check2(s1, 4_" \xFF                  ")
  s4 = " \xFF"
  call check(s1, 4_" \xFF                  ")
  call check2(s1, 4_" \xFF                  ")

  s1 = "  \xFF"
  s4 = s1
  call check(s1, 4_"  \xFF                 ")
  call check2(s1, 4_"  \xFF                 ")
  s4 = "  \xFF"
  call check(s1, 4_"  \xFF                 ")
  call check2(s1, 4_"  \xFF                 ")

contains
  subroutine check(s1,s4)
    character(kind=1,len=20) :: s1, t1
    character(kind=4,len=20) :: s4
    t1 = s4
    if (t1 /= s1) STOP 1
    if (len(s1) /= len(t1)) STOP 2
    if (len(s1) /= len(s4)) STOP 3
    if (len_trim(s1) /= len_trim(t1)) STOP 4
    if (len_trim(s1) /= len_trim(s4)) STOP 5
  end subroutine check

  subroutine check2(s1,s4)
    character(kind=1,len=*) :: s1
    character(kind=4,len=*) :: s4
    character(kind=1,len=len(s1)) :: t1
    character(kind=4,len=len(s4)) :: t4

    t1 = s4
    t4 = s1
    if (t1 /= s1) STOP 6
    if (t4 /= s4) STOP 7
    if (len(s1) /= len(t1)) STOP 8
    if (len(s1) /= len(s4)) STOP 9
    if (len(s1) /= len(t4)) STOP 10
    if (len_trim(s1) /= len_trim(t1)) STOP 11
    if (len_trim(s1) /= len_trim(s4)) STOP 12
    if (len_trim(s1) /= len_trim(t4)) STOP 13
  end subroutine check2

end
