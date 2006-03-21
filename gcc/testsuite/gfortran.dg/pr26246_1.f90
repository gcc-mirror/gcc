! PR fortran/26246
! { dg-options "-fdump-tree-original" }
! { dg-do compile }

module pr26246_1
  implicit none
  contains
    function foo(string)
    character(*), intent(in) :: string
    character(len=len(string)+2) :: foo
    if (index(trim(string), '"').ne.0) then
      foo = "'" // trim(string) // "'"
    end if
  end function foo
end module pr26246_1

! { dg-final { scan-tree-dump-times "static int" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
! { dg-final { cleanup-modules "pr26246_1" } }
