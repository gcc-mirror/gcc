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
    if (t1 /= s1) call abort
    if (len(s1) /= len(t1)) call abort
    if (len(s1) /= len(s4)) call abort
    if (len_trim(s1) /= len_trim(t1)) call abort
    if (len_trim(s1) /= len_trim(s4)) call abort
  end subroutine check

  subroutine check2(s1,s4)
    character(kind=1,len=*) :: s1
    character(kind=4,len=*) :: s4
    character(kind=1,len=len(s1)) :: t1
    character(kind=4,len=len(s4)) :: t4

    t1 = s4
    t4 = s1
    if (t1 /= s1) call abort
    if (t4 /= s4) call abort
    if (len(s1) /= len(t1)) call abort
    if (len(s1) /= len(s4)) call abort
    if (len(s1) /= len(t4)) call abort
    if (len_trim(s1) /= len_trim(t1)) call abort
    if (len_trim(s1) /= len_trim(s4)) call abort
    if (len_trim(s1) /= len_trim(t4)) call abort
  end subroutine check2

end
