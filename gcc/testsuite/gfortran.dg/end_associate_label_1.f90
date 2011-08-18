! { dg-do compile }
!
! PR fortran/50071
! A label in an END ASSOCIATE statement was ignored; as a result, a GOTO
! to such a label was rejected.
!
! Contributed by Tobias Burnus <burnus@net-b.de>

   integer :: i
   associate (j => i)
     goto 1
     print *, 'Hello'
1  end associate
end
