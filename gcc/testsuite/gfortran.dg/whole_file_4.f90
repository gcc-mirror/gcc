! { dg-do compile }
! { dg-options "-fwhole-file -std=legacy" }
! Tests the fix for PR24886 in which the mismatch between the
! character lengths of the actual and formal arguments of
! 'foo' was not detected.
!
! Contributed by Uttam Pawar <uttamp@us.ibm.com>
!
        subroutine foo(y)
           character(len=20) :: y
           y = 'hello world'
        end

        program test
           character(len=10) :: x
           call foo(x) ! { dg-warning "actual argument shorter" }
           write(*,*) 'X=',x
           pause
        end
