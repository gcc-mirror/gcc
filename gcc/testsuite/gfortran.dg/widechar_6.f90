! { dg-do run }

module mod

  interface cut
    module procedure cut1
    module procedure cut4
  end interface cut

contains

  function cut1 (s)
    character(kind=1,len=*), intent(in) :: s
    character(kind=1,len=max(0,len(s)-3)) :: cut1

    cut1 = s(4:)
  end function cut1

  function cut4 (s)
    character(kind=4,len=*), intent(in) :: s
    character(kind=4,len=max(0,len(s)-3)) :: cut4

    cut4 = s(4:)
  end function cut4

end module mod

program test
  use mod

  if (len (cut1("")) /= 0 .or. cut1("") /= "") STOP 1
  if (len (cut1("1")) /= 0 .or. cut1("") /= "") STOP 2
  if (len (cut1("12")) /= 0 .or. cut1("") /= "") STOP 3
  if (len (cut1("123")) /= 0 .or. cut1("") /= "") STOP 4
  if (len (cut1("1234")) /= 1 .or. cut1("4") /= "") STOP 5
  if (len (cut1("12345")) /= 2 .or. cut1("45") /= "") STOP 6

  if (len (cut4(4_"")) /= 0 .or. cut4(4_"") /= 4_"") STOP 7
  if (len (cut4(4_"1")) /= 0 .or. cut4(4_"") /= 4_"") STOP 8
  if (len (cut4(4_"12")) /= 0 .or. cut4(4_"") /= 4_"") STOP 9
  if (len (cut4(4_"123")) /= 0 .or. cut4(4_"") /= 4_"") STOP 10
  if (len (cut4(4_"1234")) /= 1 .or. cut4(4_"4") /= 4_"") STOP 11
  if (len (cut4(4_"12345")) /= 2 .or. cut4(4_"45") /= 4_"") STOP 12

  if (kind (cut("")) /= kind("")) STOP 13
  if (kind (cut(4_"")) /= kind(4_"")) STOP 14

  if (len (cut("")) /= 0 .or. cut("") /= "") STOP 15
  if (len (cut("1")) /= 0 .or. cut("") /= "") STOP 16
  if (len (cut("12")) /= 0 .or. cut("") /= "") STOP 17
  if (len (cut("123")) /= 0 .or. cut("") /= "") STOP 18
  if (len (cut("1234")) /= 1 .or. cut("4") /= "") STOP 19
  if (len (cut("12345")) /= 2 .or. cut("45") /= "") STOP 20

  if (len (cut(4_"")) /= 0 .or. cut(4_"") /= 4_"") STOP 21
  if (len (cut(4_"1")) /= 0 .or. cut(4_"") /= 4_"") STOP 22
  if (len (cut(4_"12")) /= 0 .or. cut(4_"") /= 4_"") STOP 23
  if (len (cut(4_"123")) /= 0 .or. cut(4_"") /= 4_"") STOP 24
  if (len (cut(4_"1234")) /= 1 .or. cut(4_"4") /= 4_"") STOP 25
  if (len (cut(4_"12345")) /= 2 .or. cut(4_"45") /= 4_"") STOP 26

end program test
