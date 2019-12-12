! { dg-do compile }
! { dg-options "-std=f2018" }
!
! PR88248 - [F18] Bogus warning about obsolescent feature: Labeled DO statement
!
program pr88248
  character*80 text     ! { dg-warning "Old-style character length" }
  f(x) = x              ! { dg-warning "Statement function" }
  call foo (*99)        ! { dg-warning "Alternate-return argument" }
  data y / 1.0 /        ! { dg-warning "DATA statement" }
  goto (1,99) i+1       ! { dg-warning "Computed GOTO" }
  ! No warning should be printed below
  goto 1
1 continue
  open (10, err=99)
  close (10, err=99)
  backspace (10, err=99)
  endfile (10, err=99)
  rewind (10, err=99)
  flush (10, err=99)
  inquire (10, err=99)
  read (*, end=99) text
99 continue
end

subroutine foobar ()
entry bar ()            ! { dg-warning "ENTRY statement" }
end subroutine foobar
