! { dg-do run }
!
! Test the fix for PR37723 in which the array element reference masked the dependency
! by inhibiting the test.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
      program try_cg0071
      type seq
          integer ia(10)
      end type
      TYPE(SEQ) UDA1R
      type(seq) uda(1)

      do j1 = 1,10
        uda1r%ia(j1) = j1
      enddo

      uda = uda1r
      UDA(1)%IA(1:9) = UDA(1)%IA(9:1:-1)+1

      DO J1 = 1,9
         if (UDA1R%IA(10-J1)+1 /=  Uda(1)%IA(J1)) call abort()
      ENDDO

      end


