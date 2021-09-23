! { dg-do compile }
! { dg-options "-fcoarray=single" }
! 
! Coarray support
! PR fortran/18918

implicit none
integer :: n, m(1), k
character(len=30) :: str(2)

critical fkl ! { dg-error "Syntax error in CRITICAL" }
end critical fkl ! { dg-error "Expecting END PROGRAM" }

sync all (stat=1) ! { dg-error "Non-variable expression" }
sync all ( stat = n,stat=k) ! { dg-error "Redundant STAT" }
sync memory (errmsg=str) ! { dg-error "must be a scalar CHARACTER variable" }
sync memory (errmsg=n) ! { dg-error "must be a scalar CHARACTER variable" }
sync images (*, stat=1.0) ! { dg-error "must be a scalar INTEGER variable" }
sync images (-1) ! { dg-error "must between 1 and num_images" }
sync images (1)
sync images ( [ 1 ])
sync images ( m(1:0) ) 
sync images ( reshape([1],[1,1])) ! { dg-error "must be a scalar or rank-1" }
end

subroutine foo
critical
  stop 'error' ! { dg-error "Image control statement STOP" }
  sync all     ! { dg-error "Image control statement SYNC" }
  return 1     ! { dg-error "Image control statement RETURN" }
  critical     ! { dg-error "Nested CRITICAL block" }
  end critical 
end critical   ! { dg-error "Expecting END SUBROUTINE" }
end

subroutine bar()
do
  critical
    cycle ! { dg-error "leaves CRITICAL construct" }
  end critical
end do

outer: do
  critical
    do
      exit
      exit outer ! { dg-error "leaves CRITICAL construct" }
    end do
  end critical
end do outer
end subroutine bar


subroutine sub()
333 continue ! { dg-error "leaves CRITICAL construct" }
do
  critical
    if (.false.) then
      goto 333 ! { dg-error "leaves CRITICAL construct" }
      goto 777
777 end if
  end critical
end do

if (.true.) then
outer: do
  critical
    do
      goto 444
      goto 555 ! { dg-error "leaves CRITICAL construct" }
    end do
444 continue
  end critical
 end do outer
555 end if ! { dg-error "leaves CRITICAL construct" }
end subroutine sub

pure subroutine pureSub()
  critical ! { dg-error "Image control statement CRITICAL" }
  end critical ! { dg-error "Expecting END SUBROUTINE statement" }
  sync all ! { dg-error "Image control statement SYNC" }
  error stop
end subroutine pureSub


SUBROUTINE TEST
   goto 10 ! { dg-warning "is not in the same block" }
   CRITICAL
     goto 5  ! OK
5    continue ! { dg-warning "is not in the same block" }
     goto 10 ! OK
     goto 20 ! { dg-error "leaves CRITICAL construct" }
     goto 30 ! { dg-error "leaves CRITICAL construct" }
10 END CRITICAL ! { dg-warning "is not in the same block" }
   goto 5 ! { dg-warning "is not in the same block" }
20 continue ! { dg-error "leaves CRITICAL construct" }
   BLOCK
30   continue ! { dg-error "leaves CRITICAL construct" }
   END BLOCK
end SUBROUTINE TEST
