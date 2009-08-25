! { dg-do compile }
! { dg-options "-std=f95" }
! PR35754 -std=f95: Reject "1P2E12.4" w/o a comma after the "P" 
! PR
! Test case provided by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      character(40) :: fmt_string
      write(*, '(1P2E12.4)') 1.0 ! { dg-error "Comma required" }
      write(*, '(1PT12,F12.4)') 1.0 ! { dg-error "Comma required" }
      write(*, '(1PE12.4)') 1.0 ! This is OK by the standard 10.1.1
      write (*,'(1PD24.15,F4.2,0P)') 1.0d0 ! This OK too.
      end
