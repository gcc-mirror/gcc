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
  character                            :: c  =   "1"
  character      (kind=4)              :: c4 = 4_"4"
  character(len=3)                     :: d  =   "210"
  character(len=3,kind=4)              :: d4 = 4_"321"
  integer :: a = 65
  integer :: l = 2
  allocate (ca, cp, ca4, cp4)

  ! Check len=1 actual argument cases first
  ca  =   "a"; cp  =   "b"; cd  =   "c"
  ca4 = 4_"d"; cp4 = 4_"e"; cd4 = 4_"f"
  call val  ("B","B", 1, 2)
  call val  ("A",char(65), 3, 4)
  call val  ("A",char(a), 5, 6)
  call val  ("A",mychar(65), 7, 8)
  call val  ("A",mychar(a), 9, 10)
  call val  ("1",c, 11, 12)
  call val  ("1",(c), 13, 14)
  call val4 (4_"C",4_"C", 15, 16)
  call val4 (4_"A",char(65,kind=4), 17, 18)
  call val4 (4_"A",char(a, kind=4), 19, 20)
  call val4 (4_"4",c4, 21, 22)
  call val4 (4_"4",(c4), 23, 24)
  call val  (ca,ca, 25, 26)
  call val  (cp,cp, 27, 28)
  call val  (cd,cd, 29, 30)
  call val  (ca,(ca), 31, 32)
  call val4 (ca4,ca4, 33, 34)
  call val4 (cp4,cp4, 35, 36)
  call val4 (cd4,cd4, 37, 38)
  call val4 (cd4,(cd4), 39, 40)
  call sub  ("S", 41, 42)
  call sub4 (4_"T", 43, 44)

  ! Check that always the first character of the string is finally used
  call val  (  "U++",  "U--", 45, 46)
  call val4 (4_"V**",4_"V//", 47, 48)
  call sub  (  "WTY", 49, 50)
  call sub4 (4_"ZXV", 51, 52)
  call val  (  "234",  d    , 53, 54)
  call val4 (4_"345",  d4   , 55, 56)
  call val  (  "234", (d)   , 57, 58)
  call val4 (4_"345", (d4)  , 59, 60)
  call val  (  "234",  d (1:2), 61, 62)
  call val4 (4_"345",  d4(1:2), 63, 64)
  call val  (  "234",  d (1:l), 65, 66)
  call val4 (4_"345",  d4(1:l), 67, 68)
  call val  ("1",c // d, 69, 70)
  call val  ("1",trim (c // d), 71, 72)
  call val4 (4_"4",c4 // d4, 73, 74)
  call val4 (4_"4",trim (c4 // d4), 75, 76)
  cd = "gkl"; cd4 = 4_"hmn"
  call val  (cd,cd, 77, 78)
  call val4 (cd4,cd4, 79, 80)
  call sub  (cd, 81, 82)
  call sub4 (cd4, 83, 84)
  deallocate (ca, cp, ca4, cp4, cd, cd4)
contains
  subroutine val (x, c, err1, err2)
    character(kind=1), intent(in) :: x  ! control: pass by reference
    character(kind=1), value      :: c
    integer, intent(in) :: err1, err2
    print *, "by value(kind=1): ", c
    if (c /= x)   stop err1
    c = "*"
    if (c /= "*") stop err2
  end

  subroutine val4 (x, c, err1, err2)
    character(kind=4), intent(in) :: x  ! control: pass by reference
    character(kind=4), value      :: c
    integer, intent(in) :: err1, err2
    print *, "by value(kind=4): ", c
    if (c /= x)     stop err1
    c = 4_"#"
    if (c /= 4_"#") stop err2
  end

  subroutine sub (s, err1, err2)
    character(*), intent(in) :: s
    integer,      intent(in) :: err1, err2
    call val (s, s, err1, err2)
  end
  subroutine sub4 (s, err1, err2)
    character(kind=4,len=*), intent(in) :: s
    integer,                 intent(in) :: err1, err2
    call val4 (s, s, err1, err2)
  end

  character function mychar (i)
    integer, intent(in) :: i
    mychar = char (i)
  end
end
