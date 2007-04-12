! { dg-do compile }
! Verify that the compiler accepts the various legal combinations of
! using construct names.
!
! The correct behavior of EXIT and CYCLE is already established in
! the various DO related testcases, they're included here for
! completeness.
       dimension a(5)
       i = 0
       ! construct name is optional on else clauses
       ia: if (i > 0) then
          i = 1
       else
          i = 2
       end if ia
       ib: if (i < 0) then
          i = 3
       else ib
          i = 4
       end if ib
       ic: if (i < 0) then
          i = 5
       else if (i == 0) then ic
          i = 6
       else if (i == 1) then
          i =7
       else if (i == 2) then ic
          i = 8
       end if ic

       fa: forall (i=1:5, a(i) > 0)
          a(i) = 9
       end forall fa

       wa: where (a > 0)
          a = -a
       elsewhere
          wb: where (a == 0)
             a = a + 1.
          elsewhere wb
             a = 2*a
          end where wb
       end where wa

       j = 1
       sa: select case (i)
          case (1)
             i = 2
          case (2) sa
             i = 3
          case default sa
             sb: select case (j)
                case (1) sb
                   i = j
                case default
                   j = i
             end select sb
       end select sa

       da: do i=1,10
          cycle da
          cycle
          exit da
          exit
          db: do
             cycle da
             cycle db
             cycle
             exit da
             exit db
             exit
             j = i+1
          end do db
          dc: do while (j>0)
             j = j-1
          end do dc
       end do da
end
