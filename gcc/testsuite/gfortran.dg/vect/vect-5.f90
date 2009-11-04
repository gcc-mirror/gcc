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

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } }
! { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail { vect_no_align || {! vector_alignment_reachable} } } } }
! { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { xfail { vect_no_align } } } }
! { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 2 "vect" { target { vect_no_align } } } }
! { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 1 "vect" { target { {! vector_alignment_reachable} && {! vect_hw_misalign} } } } }
! { dg-final { cleanup-tree-dump "vect" } }
