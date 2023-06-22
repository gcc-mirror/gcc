! { dg-do run }
! PR fortran/110360 - ABI for scalar character(len=1),value dummy argument

program p
  implicit none
  character,               allocatable :: ca
  character,               pointer     :: cp
  character(len=:),        allocatable :: cd
  character      (kind=4), allocatable :: ca4
  character      (kind=4), pointer     :: cp4
  character(len=:,kind=4), allocatable :: cd4
  integer :: a = 65
  allocate (ca, cp, ca4, cp4)

  ! Check len=1 actual argument cases first
  ca  =   "a"; cp  =   "b"; cd  =   "c"
  ca4 = 4_"d"; cp4 = 4_"e"; cd4 = 4_"f"
  call val  ("B","B")
  call val  ("A",char(65))
  call val  ("A",char(a))
  call val  ("A",mychar(65))
  call val  ("A",mychar(a))
  call val4 (4_"C",4_"C")
  call val4 (4_"A",char(65,kind=4))
  call val4 (4_"A",char(a, kind=4))
  call val  (ca,ca)
  call val  (cp,cp)
  call val  (cd,cd)
  call val4 (ca4,ca4)
  call val4 (cp4,cp4)
  call val4 (cd4,cd4)
  call sub  ("S")
  call sub4 (4_"T")

  ! Check that always the first character of the string is finally used
  call val  (  "U++",  "U--")
  call val4 (4_"V**",4_"V//")
  call sub  (  "WTY")
  call sub4 (4_"ZXV")
  cd = "gkl"; cd4 = 4_"hmn"
  call val  (cd,cd)
  call val4 (cd4,cd4)
  call sub  (cd)
  call sub4 (cd4)
  deallocate (ca, cp, ca4, cp4, cd, cd4)
contains
  subroutine val (x, c)
    character(kind=1), intent(in) :: x  ! control: pass by reference
    character(kind=1), value      :: c
    print *, "by value(kind=1): ", c
    if (c /= x)   stop 1
    c = "*"
    if (c /= "*") stop 2
  end

  subroutine val4 (x, c)
    character(kind=4), intent(in) :: x  ! control: pass by reference
    character(kind=4), value      :: c
    print *, "by value(kind=4): ", c
    if (c /= x)     stop 3
    c = 4_"#"
    if (c /= 4_"#") stop 4
  end

  subroutine sub (s)
    character(*), intent(in) :: s
    call val (s, s)
  end
  subroutine sub4 (s)
    character(kind=4,len=*), intent(in) :: s
    call val4 (s, s)
  end

  character function mychar (i)
    integer, intent(in) :: i
    mychar = char (i)
  end
end
