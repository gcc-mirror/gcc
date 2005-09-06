! { dg-require-effective-target vect_int }

        Subroutine foo (N, M)
        Integer N
        Integer M
        integer A(8,16)
        integer B(8)

        B = (/ 2, 3, 5, 7, 11, 13, 17, 23 /)

        ! Unknown loop bound. J depends on I.

        do I = 1, N
          do J = I, M
            A(J,2) = B(J)
          end do
        end do

        do I = 1, N
          do J = I, M
            if (A(J,2) /= B(J)) then
              call abort ()
              endif
          end do
        end do

        Return
        end


        program main

        Call foo (16, 8)

        stop
        end

! { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { xfail { lp64 } } } }
! { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail { vect_no_align || lp64 } } } }
! { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { xfail { vect_no_align || lp64 } } } }
! { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 3 "vect" {target vect_no_align } } }

! We also expect to vectorize one loop for lp64 targets that support 
! misaligned access:
!   scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { lp64 && !vect_no_align } }
!   scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { target { lp64 && !vect_no_align } }
!   scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { target { lp64 && !vect_no_align } }
! but we currently can't combine logical operators. (Could define 
! a keyword for "not_vect_no_align" if desired). 

! { dg-final { cleanup-tree-dump "vect" } }
