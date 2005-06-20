! { dg-do compile }
! PR 21257
program dups

  integer i,j,k

  abc: do i = 1, 3
  abc:    do j = 1, 3    ! { dg-error "Duplicate construct label" }
              k = i + j
          end do abc
       end do abc        ! { dg-error "Expecting END PROGRAM" }

  xyz: do i = 1, 2
          k = i + 2
       end do xyz
  xyz: do j = 1, 5      ! { dg-error "Duplicate construct label" }
          k = j + 2
       end do loop      ! { dg-error "Expecting END PROGRAM" }

  her: if (i == 1) then
  her:    if (j  == 1) then  ! { dg-error "Duplicate construct label" }
               k = i + j
          end if her
       end if her            ! { dg-error "Expecting END PROGRAM" }

  his: if (i == 1) then
           i = j
        end if his
  his: if (j === 1) then    ! { dg-error "Duplicate construct label" }
           print *, j
        end if his          ! { dg-error "Expecting END PROGRAM" }

  sgk: select case (i)
       case (1)
  sgk:   select case (j)    ! { dg-error "Duplicate construct label" }
          case (10)
             i = i + j
          case (20)
             j = j + i
          end select sgk
        case (2)             ! { dg-error "Unexpected CASE statement" }
            i = i + 1
            j = j + 1
        end select sgk       ! { dg-error "Expecting END PROGRAM" }

  apl: select case (i)
         case (1)
            k = 2
         case (2)
            j = 1
         end select apl
  apl: select case (i)  ! { dg-error "Duplicate construct label" }
         case (1)       ! { dg-error "Unexpected CASE statement" }
            j = 2
         case (2)       ! { dg-error "Unexpected CASE statement" }
            k = 1
         end select apl ! { dg-error "Expecting END PROGRAM" }

end program dups
