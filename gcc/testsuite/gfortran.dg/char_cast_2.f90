! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! This is the same as achar_4.f90 but checks that the result of the 'merge'
! reference is correctly cast.
!
! The code comes from http://www.star.le.ac.uk/~cgp/fortran.html (by Clive Page)
! Reported by Thomas Koenig <tkoenig@gcc.gnu.org>
!
  if (any (Up ("AbCdEfGhIjKlM") .ne. (/"ABCDEFGHIJKLM"/))) STOP 1
contains
  Character (len=20) Function Up (string)
    Character(len=*) string
    Up =                                                                &
     transfer(merge(achar(iachar(transfer(string,"x",len(string)))-     &
     (ichar('a')-ichar('A')) ),                                         &
     transfer(string,"x",len(string)) ,                                 &
     transfer(string,"x",len(string)) >= "a" .and.                      &
     transfer(string,"x",len(string)) <= "z"), repeat("x", len(string)))
    return
  end function Up
end
! The sign that all is well is that [S.5][1] appears twice.
! Platform dependent variations are [S$5][1], [__S_5][1], [S___5][1]
! so we count the occurrences of 5][1].
! { dg-final { scan-tree-dump-times "5\\\]\\\[1\\\]" 2 "original" } }
