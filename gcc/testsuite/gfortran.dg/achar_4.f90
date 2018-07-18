! { dg-do run }
! { dg-options "-fbounds-check" }
! Tests the fix for PR31257, in which achar caused an ICE because it had no
! charlen.
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
