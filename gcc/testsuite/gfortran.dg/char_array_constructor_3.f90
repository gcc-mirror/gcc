! { dg-do compile }
! tests the fix for PR32156, in which the character length of the compound
! expression got lost.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
write (*,'(2A3)') 'X'//(/"1","2"/)//'Y'
END
