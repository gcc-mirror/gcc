! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
!
! Check the fix for PR31608 in all it's various manifestations:)
! Contributed by Richard Guenther <rguenth@gcc.gnu.org>
!
  character(len=1) :: string = "z"
  integer :: i(1) = (/100/)
  print *, Up("abc")
  print *, transfer(((transfer(string,"x",1))), "x",1)
  print *, transfer(char(i), "x")
  print *, Upper ("abcdefg")
 contains
  Character (len=20) Function Up (string)
    Character(len=*) string
    character(1) :: chr
    Up = transfer(achar(iachar(transfer(string,chr,1))), "x")
    return
  end function Up
  Character (len=20) Function Upper (string)
    Character(len=*) string
    Upper =                                                                &
     transfer(merge(transfer(string,"x",len(string)),    &
       string, .true.), "x")
    return
  end function Upper
end
! The sign that all is well is that [S.6][1] appears twice.
! Platform dependent variations are [S$6][1], [__S_6][1], [S___6][1]
! { dg-final { scan-tree-dump-times "6\\\]\\\[1\\\]" 2 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
