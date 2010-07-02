! Test runtime warnings using non-standard $ editing - PR20006.
!
! Contributor Francois-Xavier Coudert  <coudert@clipper.ens.fr>
!
! { dg-options "-pedantic" }
! { dg-do run }
!
     character(5) c
     open (42,status='scratch')
     write (42,'(A,$)') 'abc' ! { dg-warning ".*descriptor" "" }
     write (42,'(A)') 'de'
     rewind (42)
     read (42,'(A)') c
     close (42)
     if (c /= 'abcde') call abort ()
     end

