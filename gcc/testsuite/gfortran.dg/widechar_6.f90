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

  if (len (cut1("")) /= 0 .or. cut1("") /= "") call abort
  if (len (cut1("1")) /= 0 .or. cut1("") /= "") call abort
  if (len (cut1("12")) /= 0 .or. cut1("") /= "") call abort
  if (len (cut1("123")) /= 0 .or. cut1("") /= "") call abort
  if (len (cut1("1234")) /= 1 .or. cut1("4") /= "") call abort
  if (len (cut1("12345")) /= 2 .or. cut1("45") /= "") call abort

  if (len (cut4(4_"")) /= 0 .or. cut4(4_"") /= 4_"") call abort
  if (len (cut4(4_"1")) /= 0 .or. cut4(4_"") /= 4_"") call abort
  if (len (cut4(4_"12")) /= 0 .or. cut4(4_"") /= 4_"") call abort
  if (len (cut4(4_"123")) /= 0 .or. cut4(4_"") /= 4_"") call abort
  if (len (cut4(4_"1234")) /= 1 .or. cut4(4_"4") /= 4_"") call abort
  if (len (cut4(4_"12345")) /= 2 .or. cut4(4_"45") /= 4_"") call abort

  if (kind (cut("")) /= kind("")) call abort
  if (kind (cut(4_"")) /= kind(4_"")) call abort

  if (len (cut("")) /= 0 .or. cut("") /= "") call abort
  if (len (cut("1")) /= 0 .or. cut("") /= "") call abort
  if (len (cut("12")) /= 0 .or. cut("") /= "") call abort
  if (len (cut("123")) /= 0 .or. cut("") /= "") call abort
  if (len (cut("1234")) /= 1 .or. cut("4") /= "") call abort
  if (len (cut("12345")) /= 2 .or. cut("45") /= "") call abort

  if (len (cut(4_"")) /= 0 .or. cut(4_"") /= 4_"") call abort
  if (len (cut(4_"1")) /= 0 .or. cut(4_"") /= 4_"") call abort
  if (len (cut(4_"12")) /= 0 .or. cut(4_"") /= 4_"") call abort
  if (len (cut(4_"123")) /= 0 .or. cut(4_"") /= 4_"") call abort
  if (len (cut(4_"1234")) /= 1 .or. cut(4_"4") /= 4_"") call abort
  if (len (cut(4_"12345")) /= 2 .or. cut(4_"45") /= 4_"") call abort

end program test
