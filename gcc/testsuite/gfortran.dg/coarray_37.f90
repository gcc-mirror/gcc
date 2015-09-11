! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
      program cg
        implicit none
        integer reduce_recv_starts(2)[1,0:*]
        interface
          subroutine conj_grad (reduce_recv_starts) ! { dg-warning "Interface mismatch in global procedure 'conj_grad' at \\(1\\): Corank mismatch in argument 'reduce_recv_starts' \\(2/1\\)" }
            integer   reduce_recv_starts(2)[2, 2:*]
          end subroutine
        end interface
        call conj_grad (reduce_recv_starts) ! Corank mismatch is okay
      end

      subroutine conj_grad (reduce_recv_starts)
        implicit none
        integer reduce_recv_starts(2)[2:*]
      end
